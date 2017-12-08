module Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import Dict
import RemoteData exposing (WebData)
import RemoteData.Http
import Random.Pcg as Random exposing (Seed)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


--

import Helper exposing (decodeTuple2, encodeTuple2)
import Crdt.ORSet as ORSet exposing (ORSet)
import Crdt.ORDict as ORDict exposing (ORDict)


type alias SyncData =
    { id : String, knownIds : ORDict String ( Int, String ), synchedWith : Set String }


init : Seed -> String -> SyncData
init seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid ( 0, "" ), synchedWith = Set.empty, id = uuid }


removeDevice : msg -> String -> SyncData -> ( SyncData, Cmd msg )
removeDevice msg uuid sync =
    ( { sync | knownIds = ORDict.remove uuid sync.knownIds, synchedWith = Set.empty }
    , informOfRemove msg uuid
    )


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    { sync | knownIds = ORDict.reset sync.knownIds, synchedWith = Set.empty }


informOfRemove : msg -> String -> Cmd msg
informOfRemove msg uuid =
    Http.post (apiUrl "/removeDevice")
        (Http.jsonBody
            (JE.object [ ( "otherId", JE.string uuid ) ])
        )
        (JD.succeed ())
        |> Http.send (always msg)


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    { sync | knownIds = ORDict.update sync.id (\( n, _ ) -> ( n + 1, newName )) sync.knownIds, synchedWith = Set.empty }


pairedWith : String -> SyncData -> SyncData -> SyncData
pairedWith uuid hisSync mySync =
    case Dict.get uuid (ORDict.get hisSync.knownIds) of
        Just v ->
            { mySync | knownIds = ORDict.insert uuid v mySync.knownIds }

        Nothing ->
            mySync


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA
-}
merge : SyncData -> SyncData -> SyncData
merge other my =
    let
        newData =
            ORDict.merge
                (\( na, a ) ( nb, b ) ->
                    if na >= nb then
                        ( na, a )
                    else
                        ( nb, b )
                )
                other.knownIds
                my.knownIds
    in
        { my
            | knownIds = newData
            , synchedWith =
                (if not <| ORDict.equal newData my.knownIds then
                    Set.empty
                 else
                    (if ORDict.equal other.knownIds newData then
                        my.synchedWith
                     else
                        Set.remove other.id my.synchedWith
                    )
                )
                    |> (\sWith ->
                            if ORDict.equal newData other.knownIds then
                                Set.insert other.id sWith
                            else
                                sWith
                       )
        }


syncToOthers : msg -> SyncData -> ( SyncData, Cmd msg )
syncToOthers msg sync =
    let
        contactSet =
            Set.diff (ORDict.get sync.knownIds |> Dict.keys |> Set.fromList) (Set.insert sync.id sync.synchedWith)
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
        (JD.field "knownIds" <| ORDict.decoder (decodeTuple2 JD.int JD.string))
        (JD.succeed Set.empty)


syncDataEncoder : SyncData -> Value
syncDataEncoder s =
    JE.object [ ( "knownIds", ORDict.encode (encodeTuple2 JE.int JE.string) s.knownIds ), ( "id", JE.string s.id ) ]


endPointUrl : String -> String -> String
endPointUrl pre path =
    -- TODO: change
    -- "localhost"
    -- {- etz upper -}
    -- "10.2.117.8"
    {- etz lower -}
    "10.2.122.231"
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
                            (JD.field "syncData" syncDataDecoder)

                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" syncDataDecoder)

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
