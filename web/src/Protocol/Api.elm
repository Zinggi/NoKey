module Protocol.Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Task exposing (Task)
import Time exposing (Time)
import Dict exposing (Dict)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Crdt.VClock as VClock exposing (VClock)


--

import Helper exposing (encodeTuple, decodeTuple, mapModel, noCmd, performWithTimestamp, withCmds, attemptWithTimestamp)
import Data.Sync exposing (SyncData, OtherSharedData)
import SecretSharing
import Views.Pairing


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



   # Notse
   ## Pairing
   There is no way to be completely sure wheater we finished the pairing process.
   Lets say A sends to B pairedWith(A,B). At this point A cannot commit to pairing, as it doesn't know wheater B knows.
   Now B sends back ok. B cannot commit to pairing, as it doesn't know if A knows that B knows.
   We don't know when we can stop.
   => https://en.wikipedia.org/wiki/Two_Generals'_Problem

-}


endPointUrl : String -> String -> String
endPointUrl pre path =
    -- TODO: change
    -- "localhost"
    {- etz upper -}
    "10.2.120.53"
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



-- Socket


connectPrivateSocket : String -> Sub Msg
connectPrivateSocket uuid =
    let
        socket =
            Socket.init socketUrl
                |> Socket.heartbeatIntervallSeconds 10

        channel =
            Channel.init ("private:" ++ uuid)
                -- register a handler for messages with a "new_msg" event
                |> Channel.on "new_msg" jsonToMsg
                |> Channel.withDebug
                |> Channel.withPayload (JE.object [ ( "uuid", JE.string uuid ) ])
                |> Channel.onJoin JoinedChannel
    in
        Phoenix.connect socket [ channel ]



--


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


type alias State =
    { pairingState : PairingState, pairing : Views.Pairing.State }


type PairingState
    = Init
    | WaitForPaired Time String
    | WaitForFinished Time String String


init : State
init =
    { pairingState = Init, pairing = Views.Pairing.init }


type Msg
    = Server ServerMsg
    | Authenticated String AuthenticatedMsg
    | NoReply
    | DecodeError String
    | JoinedChannel JE.Value


type ServerMsg
    = ReceiveToken Time (WebData String)
    | PairedWith (Result Http.Error String)


type AuthenticatedMsg
    = ReceiveMessage JE.Value
    | DecodeReceivedMessage JE.Value Time
    | JoinChannel JE.Value
    | SyncUpdate OtherSharedData
    | RequestShare ( String, String )
    | GrantedShareRequest ( String, String ) SecretSharing.Share
    | GotRemoved
    | NeedsUpdate VClock



-- TODO: finish state machine..


update : SyncData -> State -> Msg -> ( State, Cmd Msg )
update sync state msg =
    case ( msg, state.pairingState ) of
        ( Server (ReceiveToken time maybeToken), Init ) ->
            (case maybeToken of
                Success token ->
                    { state | pairingState = WaitForPaired time token }

                _ ->
                    state
            )
                |> (\m -> { m | pairing = Views.Pairing.receivedToken maybeToken m.pairing })
                |> noCmd

        ( Server (PairedWith res), WaitForPaired t0 token ) ->
            -- TODO: send FinishPairing
            (case res of
                Ok otherId ->
                    ( { state | pairingState = WaitForFinished t0 token otherId }, finishPairing otherId token sync )

                Err e ->
                    ( { state | pairingState = Init }, Cmd.none )
            )
                |> mapModel (\m -> { m | pairing = Views.Pairing.pairingCompleted res m.pairing })

        ( JoinedChannel v, _ ) ->
            let
                _ =
                    Debug.log "(re)join channel" v
            in
                state |> withCmds [ askForNewVersion NoReply sync ]

        ( NoReply, _ ) ->
            state |> noCmd


{-| The pairing works as follows:
TODO: add time of start, at finish, check if <60s
We start the process with initPairing. This send our Id to the server, which replies with a random token.
We enter the token on another client and send it to the server. (pairWith)
The server sends both devices the id of the other device back. (PairedWith)
On receiving PairedWith, they send (FinishPairing token sync) to each other.
On receiving (FinishPairing token sync), check if the token matches, if yes paired and end with a syncUpdate
-}
initPairing : String -> State -> ( State, Cmd Msg )
initPairing uuid state =
    ( { state | pairing = Views.Pairing.getTockenClicked state.pairing, pairingState = Init }
    , (RemoteData.Http.postTask (apiUrl "/initPairing")
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ) ])
      )
        |> performWithTimestamp ReceiveToken
        |> Cmd.map Server
    )


pairWith : String -> Time -> State -> ( State, Cmd Msg )
pairWith myId time state =
    { state | pairingState = WaitForPaired time state.pairing.inputToken }
        |> withCmds
            [ Http.post (apiUrl "/pairWith")
                (Http.jsonBody
                    (JE.object
                        [ ( "deviceId", JE.string myId )
                        , ( "token", JE.string state.pairing.inputToken )
                        ]
                    )
                )
                (JD.field "otherId" JD.string)
                |> Http.toTask
                |> Task.attempt PairedWith
                |> Cmd.map Server
            ]


finishPairing : String -> String -> SyncData -> Cmd Msg
finishPairing otherId token sync =
    sendMsgTo NoReply sync.id otherId "FinishPairing" [ ( "token", JE.string token ), ( "sync", Data.Sync.encode sync ) ]



-- Shares


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



--


jsonToMsg : Value -> Msg
jsonToMsg msg =
    case JD.decodeValue serverResponseDecoder msg of
        Ok m ->
            m

        Err e ->
            DecodeError e


serverResponseDecoder : JD.Decoder Msg
serverResponseDecoder =
    (JD.map2 (,)
        (JD.field "from" JD.string)
        (JD.field "type" JD.string)
    )
        |> JD.andThen
            (\( id, t ) ->
                case t of
                    "PairedWith" ->
                        JD.succeed (PairedWith (Ok id))
                            |> JD.map Server

                    -- TODO: these messages should be authenticated with an HMAC
                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" (Data.Sync.decoder id))
                            |> JD.map (Authenticated id)

                    "GotRemoved" ->
                        JD.succeed GotRemoved
                            |> JD.map (Authenticated id)

                    "RequestShare" ->
                        JD.map RequestShare (JD.field "shareId" (decodeTuple JD.string))
                            |> JD.map (Authenticated id)

                    "GrantedShareRequest" ->
                        JD.map2 GrantedShareRequest
                            (JD.field "shareId" (decodeTuple JD.string))
                            (JD.field "share" SecretSharing.shareDecoder)
                            |> JD.map (Authenticated id)

                    "NeedsUpdate" ->
                        JD.map NeedsUpdate (JD.field "version" VClock.decoder)
                            |> JD.map (Authenticated id)

                    other ->
                        JD.fail ("no recognized type: " ++ other)
            )
