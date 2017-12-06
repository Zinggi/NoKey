module Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import RemoteData exposing (WebData)
import RemoteData.Http


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


--

import Crdt.ORSet as ORSet exposing (ORSet)


type alias SyncData =
    { id : String, knownIds : ORSet String, synchedWith : Set String }


init : String -> SyncData
init uuid =
    { knownIds = ORSet.init |> ORSet.add uuid, synchedWith = Set.empty, id = uuid }


merge : SyncData -> SyncData -> SyncData
merge my other =
    let
        newData =
            ORSet.merge my.knownIds other.knownIds
    in
        { my
            | knownIds = newData
            , synchedWith =
                (if newData /= my.knownIds then
                    Set.empty
                 else
                    (if other.knownIds == newData then
                        my.synchedWith
                     else
                        Set.remove other.id my.synchedWith
                    )
                )
                    |> (\sWith ->
                            if newData == other.knownIds then
                                Set.insert other.id sWith
                            else
                                sWith
                       )
        }


syncToOthers : msg -> SyncData -> ( SyncData, Cmd msg )
syncToOthers msg sync =
    let
        contactSet =
            Set.diff (ORSet.get sync.knownIds) (Set.insert sync.id sync.synchedWith)
    in
        ( { sync | synchedWith = Set.union contactSet sync.synchedWith }
        , contactSet
            |> Set.foldl
                (\id acc ->
                    syncWith msg id sync :: acc
                )
                []
            |> Cmd.batch
        )


resetSynchedWith : SyncData -> SyncData
resetSynchedWith sync =
    { sync | synchedWith = Set.empty }


syncWith : msg -> String -> SyncData -> Cmd msg
syncWith msg id sync =
    Http.post (apiUrl "/syncWith")
        (Http.jsonBody
            (JE.object [ ( "syncData", syncDataEncoder sync ), ( "otherId", JE.string id ) ])
        )
        (JD.succeed ())
        |> Http.send (always msg)


syncDataDecoder : Decoder SyncData
syncDataDecoder =
    JD.map3 SyncData
        (JD.field "id" JD.string)
        (JD.field "knownIds" ORSet.decoder)
        (JD.succeed Set.empty)


syncDataEncoder : SyncData -> Value
syncDataEncoder s =
    JE.object [ ( "knownIds", ORSet.encode s.knownIds ), ( "id", JE.string s.id ) ]


endPointUrl : String -> String -> String
endPointUrl pre path =
    -- TODO: change
    -- "localhost"
    -- "10.2.117.8"
    "floyogaarch.fritz.box"
        |> (\ip -> pre ++ ip ++ ":4000" ++ path)


apiUrl : String -> String
apiUrl path =
    endPointUrl "http://" ("/api" ++ path)


socketUrl : String
socketUrl =
    endPointUrl "ws://" "/socket/websocket"


initPairing : (WebData String -> msg) -> String -> SyncData -> Cmd msg
initPairing tagger uuid syncData =
    RemoteData.Http.post (apiUrl "/initPairing")
        tagger
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ), ( "syncData", syncDataEncoder syncData ) ])


pairWith : (Result Http.Error ( String, SyncData ) -> msg) -> String -> String -> SyncData -> Cmd msg
pairWith tagger myId token syncData =
    Http.post (apiUrl "/pairWith")
        (Http.jsonBody
            (JE.object
                [ ( "deviceId", JE.string myId )
                , ( "token", JE.string token )
                , ( "syncData", syncDataEncoder syncData )
                ]
            )
        )
        (JD.map2 (,)
            (JD.field "otherId" JD.string)
            (JD.field "syncData" syncDataDecoder)
        )
        |> Http.send tagger


type ServerResponse
    = PairedWith String SyncData
    | SyncUpdate SyncData


serverResponseDecoder : JD.Decoder ServerResponse
serverResponseDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "PairedWith" ->
                        JD.map2 PairedWith
                            (JD.field "otherId" JD.string)
                            (JD.field "syncData" syncDataDecoder)

                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" syncDataDecoder)

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
