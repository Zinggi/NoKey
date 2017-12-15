module Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task
import Time exposing (Time)
import Dict exposing (Dict)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


--

import Helper exposing (encodeTuple, decodeTuple)
import SyncData exposing (SyncData)
import SecretSharing


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
    ( SyncData.removeDevice uuid sync
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
        (SyncData.knownOtherIds sync)
        |> Cmd.batch


syncToOthers : msg -> SyncData -> ( SyncData, Cmd msg )
syncToOthers msg sync =
    let
        contactSet =
            SyncData.getContactSet sync
    in
        ( SyncData.updateSynchedWith contactSet sync
        , contactSet
            |> Set.foldl
                (\id acc ->
                    syncWith msg sync.id id sync :: acc
                )
                []
            |> Cmd.batch
        )


syncWith : msg -> String -> String -> SyncData -> Cmd msg
syncWith msg myId otherId sync =
    sendMsgTo msg myId otherId "SyncUpdate" [ ( "syncData", SyncData.encode sync ) ]


initPairing : (WebData String -> msg) -> String -> SyncData -> Cmd msg
initPairing tagger uuid syncData =
    RemoteData.Http.post (apiUrl "/initPairing")
        tagger
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ), ( "syncData", SyncData.encode syncData ) ])


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


pairWith : (Result Http.Error ( String, SyncData, Time ) -> msg) -> String -> String -> SyncData -> Cmd msg
pairWith tagger myId token syncData =
    Http.post (apiUrl "/pairWith")
        (Http.jsonBody
            (JE.object
                [ ( "deviceId", JE.string myId )
                , ( "token", JE.string token )
                , ( "syncData", SyncData.encode syncData )
                ]
            )
        )
        (JD.map2 (,)
            (JD.field "otherId" JD.string)
            (JD.field "syncData" SyncData.decoder)
        )
        |> Http.toTask
        |> Task.andThen
            (\( id, sync ) ->
                Time.now
                    |> Task.map (\time -> ( id, sync, time ))
            )
        |> Task.attempt tagger


type ServerResponse
    = PairedWith SyncData
    | SyncUpdate SyncData
    | RequestShare ( String, String )
    | GrantedShareRequest ( String, String ) SecretSharing.Share
    | GotRemoved


serverResponseDecoder : JD.Decoder ( String, ServerResponse )
serverResponseDecoder =
    JD.map2 (,)
        (JD.field "from" JD.string)
        (JD.field "type" JD.string
            |> JD.andThen
                (\t ->
                    case t of
                        "PairedWith" ->
                            JD.map PairedWith (JD.field "syncData" SyncData.decoder)

                        "SyncUpdate" ->
                            JD.map SyncUpdate (JD.field "syncData" SyncData.decoder)

                        "GotRemoved" ->
                            JD.succeed GotRemoved

                        "RequestShare" ->
                            JD.map RequestShare (JD.field "shareId" (decodeTuple JD.string))

                        "GrantedShareRequest" ->
                            JD.map2 GrantedShareRequest
                                (JD.field "shareId" (decodeTuple JD.string))
                                (JD.field "share" SecretSharing.shareDecoder)

                        other ->
                            JD.fail ("no recognized type: " ++ other)
                )
        )


connectPrivateSocket : (JE.Value -> msg) -> (JE.Value -> msg) -> String -> Sub msg
connectPrivateSocket tagger onRejoin uuid =
    let
        socket =
            Socket.init socketUrl

        channel =
            Channel.init ("private:" ++ uuid)
                -- register a handler for messages with a "new_msg" event
                |> Channel.on "new_msg" tagger
                |> Channel.withDebug
                |> Channel.withPayload (JE.object [ ( "uuid", JE.string uuid ) ])
                |> Channel.onRejoin onRejoin
    in
        Phoenix.connect socket [ channel ]
