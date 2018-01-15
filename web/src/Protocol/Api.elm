module Protocol.Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task
import Time exposing (Time)
import Dict exposing (Dict)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Crdt.VClock as VClock exposing (VClock)


--

import Helper exposing (encodeTuple, decodeTuple)
import Data.Sync exposing (SyncData, OtherSharedData)
import SecretSharing


{- TODO: do some more exhaustive tests of performance

    ## this test tested pairing n devices and changing each name to a three word sentance.
    the minimum bandwidth would be: n*sizeOf3WordMsg + n*sizeOfPairing

   | #devices         | 1 | 2 |  3 |  4 |
   --------------------------------------
   | #bandwidth in kb | 0 | 7 | 16 | 32 |


    ## This test starts from n devices paired possition,
    then meassures how much bytes are transfered on typing a single letter as name.
    minimum bandwidth would be: n*sizeOfSmallMessage
    we seem to hit this limit

   | 1 | 2 | 3 | 4 | 5
   | 0 | 1 | 1 | 3 | 4


-}


endPointUrl : String -> String -> String
endPointUrl pre path =
    -- TODO: change
    "localhost"
        -- {- etz upper -}
        -- "10.2.117.8"
        -- {- etz lower -}
        -- "10.2.122.231"
        -- {- hg lower -}
        -- "10.2.54.70"
        -- "floyogaarch.fritz.box"
        |> (\ip -> pre ++ ip ++ ":4000" ++ path)


apiUrl : String -> String
apiUrl path =
    endPointUrl "http://" ("/api" ++ path)


socketUrl : String
socketUrl =
    endPointUrl "ws://" "/socket/websocket"


removeDevice : msg -> String -> SyncData -> ( SyncData, Cmd msg )
removeDevice msg uuid sync =
    ( Data.Sync.removeDevice uuid sync
    , informOfRemove msg sync.id uuid
    )


informOfRemove : msg -> String -> String -> Cmd msg
informOfRemove msg myId otherId =
    sendMsgTo msg myId otherId "GotRemoved" []


sendMsgTo : msg -> String -> String -> String -> List ( String, Value ) -> Cmd msg
sendMsgTo msg myId otherId type_ content =
    Http.post (apiUrl ("/sendMsgTo/" ++ otherId))
        (Http.jsonBody (JE.object (( "type", JE.string type_ ) :: ( "from", JE.string myId ) :: content)))
        (JD.succeed ())
        |> Http.send (always msg)


sendMsgToAll : msg -> SyncData -> String -> List ( String, Value ) -> Cmd msg
sendMsgToAll msg sync type_ content =
    List.map (\id -> sendMsgTo msg sync.id id type_ content)
        (Data.Sync.knownOtherIds sync)
        |> Cmd.batch


sendMsgToGroup : msg -> String -> List String -> String -> List ( String, Value ) -> Cmd msg
sendMsgToGroup msg myId otherIds type_ content =
    List.map (\id -> sendMsgTo msg myId id type_ content) otherIds |> Cmd.batch


askForNewVersion : msg -> SyncData -> Cmd msg
askForNewVersion msg sync =
    sendMsgToAll msg sync "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


askForNewVersionFrom : msg -> List String -> SyncData -> Cmd msg
askForNewVersionFrom msg otherIds sync =
    sendMsgToGroup msg sync.id otherIds "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


syncToOthers : msg -> SyncData -> ( SyncData, Cmd msg )
syncToOthers msg sync =
    let
        ( ( needMine, needTheirs ), newSync ) =
            Data.Sync.syncWithOthers sync
    in
        ( newSync
        , Cmd.batch [ syncWith msg needMine sync, askForNewVersionFrom msg needTheirs sync ]
        )


syncWith : msg -> List String -> SyncData -> Cmd msg
syncWith msg otherIds sync =
    sendMsgToGroup msg sync.id otherIds "SyncUpdate" [ ( "syncData", Data.Sync.encode sync ) ]


initPairing : (WebData String -> msg) -> String -> SyncData -> Cmd msg
initPairing tagger uuid syncData =
    RemoteData.Http.post (apiUrl "/initPairing")
        tagger
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ), ( "syncData", Data.Sync.encode syncData ) ])


requestShare : msg -> ( String, String ) -> SyncData -> Cmd msg
requestShare msg key sync =
    sendMsgToAll msg sync "RequestShare" [ ( "shareId", encodeTuple JE.string key ) ]


grantRequest : msg -> { key : ( String, String ), id : String } -> SyncData -> Cmd msg
grantRequest msg req sync =
    case Dict.get req.key sync.myShares of
        Just share ->
            sendMsgTo msg
                sync.id
                req.id
                "GrantedShareRequest"
                [ ( "share", SecretSharing.encodeShare share )
                , ( "shareId", encodeTuple JE.string req.key )
                ]

        Nothing ->
            Cmd.none


pairWith : (Result Http.Error ( String, OtherSharedData, Time ) -> msg) -> String -> String -> SyncData -> Cmd msg
pairWith tagger myId token syncData =
    Http.post (apiUrl "/pairWith")
        (Http.jsonBody
            (JE.object
                [ ( "deviceId", JE.string myId )
                , ( "token", JE.string token )
                , ( "syncData", Data.Sync.encode syncData )
                ]
            )
        )
        (JD.field "otherId" JD.string
            |> JD.andThen
                (\id ->
                    (JD.field "syncData" (Data.Sync.decoder id))
                        |> JD.map (\s -> ( id, s ))
                )
        )
        |> Http.toTask
        |> Task.andThen
            (\( id, sync ) ->
                Time.now
                    |> Task.map (\time -> ( id, sync, time ))
            )
        |> Task.attempt tagger


type ServerResponse
    = PairedWith OtherSharedData
    | SyncUpdate OtherSharedData
    | RequestShare ( String, String )
    | GrantedShareRequest ( String, String ) SecretSharing.Share
    | GotRemoved
    | NeedsUpdate VClock


decodeServerResponse : Value -> Result String ( String, ServerResponse )
decodeServerResponse msg =
    JD.decodeValue serverResponseDecoder msg


serverResponseDecoder : JD.Decoder ( String, ServerResponse )
serverResponseDecoder =
    (JD.map2 (,)
        (JD.field "from" JD.string)
        (JD.field "type" JD.string)
    )
        |> JD.andThen
            (\( id, t ) ->
                (case t of
                    "PairedWith" ->
                        JD.map PairedWith (JD.field "syncData" (Data.Sync.decoder id))

                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" (Data.Sync.decoder id))

                    "GotRemoved" ->
                        JD.succeed GotRemoved

                    "RequestShare" ->
                        JD.map RequestShare (JD.field "shareId" (decodeTuple JD.string))

                    "GrantedShareRequest" ->
                        JD.map2 GrantedShareRequest
                            (JD.field "shareId" (decodeTuple JD.string))
                            (JD.field "share" SecretSharing.shareDecoder)

                    "NeedsUpdate" ->
                        JD.map NeedsUpdate (JD.field "version" VClock.decoder)

                    other ->
                        JD.fail ("no recognized type: " ++ other)
                )
                    |> JD.map (\res -> ( id, res ))
            )


connectPrivateSocket : (JE.Value -> msg) -> (JE.Value -> msg) -> String -> Sub msg
connectPrivateSocket tagger onJoin uuid =
    let
        socket =
            Socket.init socketUrl

        channel =
            Channel.init ("private:" ++ uuid)
                -- register a handler for messages with a "new_msg" event
                |> Channel.on "new_msg" tagger
                |> Channel.withDebug
                |> Channel.withPayload (JE.object [ ( "uuid", JE.string uuid ) ])
                |> Channel.onJoin onJoin
    in
        Phoenix.connect socket [ channel ]
