module Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task
import Time exposing (Time)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


--

import SyncData exposing (SyncData)


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
    , informOfRemove msg uuid
    )


informOfRemove : msg -> String -> Cmd msg
informOfRemove msg uuid =
    sendMsgTo msg uuid "GotRemoved" []


sendMsgTo : msg -> String -> String -> List ( String, Value ) -> Cmd msg
sendMsgTo msg id type_ content =
    Http.post (apiUrl ("/sendMsgTo/" ++ id))
        (Http.jsonBody (JE.object (( "type", JE.string type_ ) :: content)))
        (JD.succeed ())
        |> Http.send (always msg)


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
                    syncWith msg id sync :: acc
                )
                []
            |> Cmd.batch
        )


syncWith : msg -> String -> SyncData -> Cmd msg
syncWith msg id sync =
    sendMsgTo msg id "SyncUpdate" [ ( "syncData", SyncData.encode sync ) ]


initPairing : (WebData String -> msg) -> String -> SyncData -> Cmd msg
initPairing tagger uuid syncData =
    RemoteData.Http.post (apiUrl "/initPairing")
        tagger
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ), ( "syncData", SyncData.encode syncData ) ])


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
    = PairedWith String SyncData
    | SyncUpdate SyncData
    | GotRemoved


serverResponseDecoder : JD.Decoder ServerResponse
serverResponseDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "PairedWith" ->
                        JD.map2 PairedWith
                            (JD.field "otherId" JD.string)
                            (JD.field "syncData" SyncData.decoder)

                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" SyncData.decoder)

                    "GotRemoved" ->
                        JD.succeed GotRemoved

                    other ->
                        JD.fail ("no recognized type: " ++ other)
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
