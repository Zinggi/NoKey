module Api exposing (..)

import Http
import Json.Decode as JD
import Json.Encode as JE
import RemoteData exposing (WebData)
import RemoteData.Http


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


endPointUrl : String -> String -> String
endPointUrl pre path =
    -- TODO: change
    "localhost"
        -- "10.2.117.8"
        |> (\ip -> pre ++ ip ++ ":4000" ++ path)


apiUrl : String -> String
apiUrl path =
    endPointUrl "http://" ("/api" ++ path)


socketUrl : String
socketUrl =
    endPointUrl "ws://" "/socket/websocket"


initPairing : (WebData String -> msg) -> String -> Cmd msg
initPairing tagger uuid =
    RemoteData.Http.post (apiUrl "/initPairing") tagger (JD.at [ "token" ] JD.string) (JE.object [ ( "deviceId", JE.string uuid ) ])


pairWith : (Result Http.Error String -> msg) -> String -> String -> Cmd msg
pairWith tagger myId token =
    Http.post (apiUrl "/pairWith") (Http.jsonBody (JE.object [ ( "deviceId", JE.string myId ), ( "token", JE.string token ) ])) (JD.at [ "otherId" ] JD.string)
        |> Http.send tagger


connectPrivateSocket : (JE.Value -> msg) -> String -> Sub msg
connectPrivateSocket tagger uuid =
    let
        socket =
            Socket.init socketUrl

        channel =
            Channel.init ("private:" ++ uuid)
                -- register a handler for messages with a "new_msg" event
                |> Channel.on "new_msg" tagger
                |> Channel.withDebug
                |> Channel.withPayload (JE.object [ ( "uuid", JE.string uuid ) ])
    in
        Phoenix.connect socket [ channel ]
