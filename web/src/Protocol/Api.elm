module Protocol.Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Task exposing (Task)
import Process
import Time exposing (Time)
import Dict exposing (Dict)


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Debounce exposing (Debounce)
import Crdt.VClock as VClock exposing (VClock)


--

import Helper exposing (..)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Notifications as Notifications
import Data.RequestPassword
import SecretSharing
import Views.Pairing
import Model exposing (Model, updateNotifications, updateProtocol, protocolMsg)
import Protocol.Data as Protocol exposing (..)
import Ports
import Data.Storage


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


connectPrivateSocket : String -> Sub Model.Msg
connectPrivateSocket uuid =
    let
        socket =
            Socket.init socketUrl
                |> Socket.heartbeatIntervallSeconds 10

        channel =
            Channel.init ("private:" ++ uuid)
                -- register a handler for messages with a "new_msg" event
                |> Channel.on "new_msg" (NewMsg >> Self >> protocolMsg)
                |> Channel.withDebug
                |> Channel.withPayload (JE.object [ ( "uuid", JE.string uuid ) ])
                |> Channel.onJoin (JoinedChannel >> Self >> protocolMsg)
    in
        Phoenix.connect socket [ channel ]



--


removeDevice : String -> SyncData -> ( SyncData, Cmd Model.Msg )
removeDevice uuid sync =
    ( Data.Sync.removeDevice uuid sync
    , informOfRemove sync.id uuid
    )


informOfRemove : String -> String -> Cmd Model.Msg
informOfRemove myId otherId =
    sendMsgTo myId otherId "GotRemoved" []


sendMsgTo : String -> String -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgTo myId otherId type_ content =
    Http.post (apiUrl ("/sendMsgTo/" ++ otherId))
        (Http.jsonBody (JE.object (( "type", JE.string type_ ) :: ( "from", JE.string myId ) :: content)))
        (JD.succeed ())
        |> Http.send (always (protocolMsg (Self NoReply)))


sendMsgToAll : SyncData -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgToAll sync type_ content =
    List.map (\id -> sendMsgTo sync.id id type_ content)
        (Data.Sync.knownOtherIds sync)
        |> Cmd.batch


sendMsgToGroup : String -> List String -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgToGroup myId otherIds type_ content =
    List.map (\id -> sendMsgTo myId id type_ content) otherIds |> Cmd.batch


askForNewVersion : SyncData -> Cmd Model.Msg
askForNewVersion sync =
    sendMsgToAll sync "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


askForNewVersionFrom : List String -> SyncData -> Cmd Model.Msg
askForNewVersionFrom otherIds sync =
    sendMsgToGroup sync.id otherIds "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


doSyncToOthers : SyncData -> ( SyncData, Cmd Model.Msg )
doSyncToOthers sync =
    let
        ( ( needMine, needTheirs ), newSync ) =
            Data.Sync.syncWithOthers sync
    in
        ( newSync
        , Cmd.batch [ syncWith needMine newSync, askForNewVersionFrom needTheirs newSync ]
        )


syncWith : List String -> SyncData -> Cmd Model.Msg
syncWith otherIds sync =
    sendMsgToGroup sync.id otherIds "SyncUpdate" [ ( "syncData", Data.Sync.encode sync ) ]


syncToOthersDebouncer : Debounce.Config Model.Msg
syncToOthersDebouncer =
    { strategy = Debounce.soon (1 * Time.second)
    , transform = SyncToOthers >> Self >> protocolMsg
    }


addSync : b -> ( a, c ) -> ( a, b, c )
addSync b ( a, c ) =
    ( a, b, c )


addOutMsg : d -> ( a, b, c ) -> ( a, b, c, Maybe d )
addOutMsg d ( a, b, c ) =
    ( a, b, c, Just d )


updateState : Model -> State -> Model
updateState model state =
    { model | protocolState = state }


toModel : Model -> ( State, Cmd Model.Msg ) -> ( Model, Cmd Model.Msg )
toModel model ( state, cmd ) =
    ( updateState model state, cmd )


update : Model -> Msg -> ( Model, Cmd Model.Msg )
update model msg =
    let
        ( state, sync ) =
            ( model.protocolState, model.syncData )

        _ =
            case msg of
                Self _ ->
                    ()

                _ ->
                    Debug.log ("\tgot msg:\n" ++ toString msg ++ "\n\tin state:\n" ++ toString state.pairingState) ()
    in
        (case ( msg, model.protocolState.pairingState ) of
            ( Server (ReceiveToken time maybeToken), Init ) ->
                (case maybeToken of
                    Success token ->
                        { state | pairingState = WaitForPaired time token }

                    _ ->
                        state
                )
                    |> updateState model
                    |> (\m -> { m | pairingDialogue = Views.Pairing.receivedToken maybeToken m.pairingDialogue })
                    |> noCmd

            ( Server (ReceiveToken _ _), _ ) ->
                Debug.crash "got token, but we were not expecting one" ( msg, state )
                    |> always ( model, Cmd.none )

            ( Server (PairedWith res), WaitForPaired t0 token ) ->
                (case res of
                    Ok otherId ->
                        ( { state | pairingState = WaitForFinished t0 token otherId }, finishPairing otherId token sync )

                    Err e ->
                        ( { state | pairingState = Init }, Cmd.none )
                )
                    |> toModel model
                    |> mapModel (\m -> { m | pairingDialogue = Views.Pairing.pairingCompleted res m.pairingDialogue })

            ( Server (PairedWith _), _ ) ->
                Debug.crash "got PairedWith, but we were not expecting it now" ( msg, state )
                    |> always ( model, Cmd.none )

            ( Authenticated otherId time (FinishPairing otherToken otherSync), WaitForPaired t0 token ) ->
                receiveFinishPairing token time otherToken otherId otherSync model

            ( Authenticated otherId time (FinishPairing otherToken otherSync), WaitForFinished t0 token _ ) ->
                receiveFinishPairing token time otherToken otherId otherSync model

            ( Authenticated _ _ (FinishPairing _ _), Init ) ->
                -- This happens when we are already paired, beacause finishPairing is sent multiple times
                ( model, Cmd.none )

            -- Independant of current state
            ( Authenticated otherId time (RequestShare key), _ ) ->
                model
                    |> updateNotifications (Notifications.newShareRequest otherId key)

            ( Authenticated otherId time (GrantedShareRequest key share), _ ) ->
                let
                    newSitesState =
                        Data.RequestPassword.addShare key share model.sitesState

                    newModel =
                        { model | sitesState = newSitesState }

                    ( site, login ) =
                        key
                in
                    case Data.RequestPassword.getStatus key newSitesState of
                        Data.RequestPassword.Done True pw ->
                            -- if done and fillForm is set, call port to fill the form
                            newModel |> withCmds [ Ports.fillForm { login = login, site = site, password = pw } ]

                        _ ->
                            newModel |> noCmd

            ( Authenticated otherId time (SyncUpdate otherSync), _ ) ->
                let
                    ( debounce, cmd ) =
                        Debounce.push syncToOthersDebouncer () state.debounce
                in
                    { state | debounce = debounce }
                        |> updateState model
                        |> (\m -> { m | syncData = Data.Sync.merge time otherSync sync })
                        |> withCmds [ cmd ]

            ( Authenticated otherId time GotRemoved, _ ) ->
                { model | syncData = Data.Sync.gotRemoved model.syncData }
                    |> syncToOthers

            ( Authenticated otherId time (NeedsUpdate version), _ ) ->
                { model | syncData = Data.Sync.receiveVersion otherId version model.syncData }
                    |> syncToOthers

            ( Self (JoinedChannel v), _ ) ->
                let
                    _ =
                        Debug.log "(re)join channel" v
                in
                    model
                        |> withCmds [ askForNewVersion sync ]

            ( Self (NewMsg v), _ ) ->
                model
                    |> withCmds [ withTimestamp (jsonToMsg v >> protocolMsg) ]

            ( Self (SyncToOthers msg), _ ) ->
                -- delay the sync update to others, as we might get multiple updates in a short time, so wait until it settled.
                -- Since we always call this when we got a new state, we also store our state here
                let
                    ( newSync, cmdToDebounce ) =
                        doSyncToOthers sync

                    ( debounce, cmd ) =
                        Debounce.update
                            syncToOthersDebouncer
                            (Debounce.takeLast (always cmdToDebounce))
                            msg
                            state.debounce

                    newModel =
                        { state | debounce = debounce }
                            |> updateState model
                            |> (\m -> { m | syncData = newSync })
                in
                    newModel
                        |> withCmds [ cmd, Ports.storeState (Data.Storage.encode newModel) ]

            ( Self (DecodeError e), _ ) ->
                Debug.crash "faild to decode msg" e
                    |> always ( model, Cmd.none )

            ( Self (Timer time), Init ) ->
                -- Ignore timer in init state
                ( model, Cmd.none )

            ( Self (Timer time), WaitForFinished t0 token otherId ) ->
                checkTimer t0 time model
                    |> addCmds [ finishPairing otherId token sync ]

            ( Self (Timer time), WaitForPaired t0 _ ) ->
                checkTimer t0 time model

            ( Self NoReply, _ ) ->
                ( model, Cmd.none )
        )
            |> andThenUpdate
                (\m ->
                    if m.protocolState.pairingState /= Init then
                        ( m, startTimer (5 * Time.second) )
                    else
                        ( m, Cmd.none )
                )


startTimer : Time -> Cmd Model.Msg
startTimer time =
    Process.sleep time
        |> Task.andThen
            (\_ ->
                Time.now
            )
        |> Task.perform (\t -> protocolMsg (Self (Timer t)))


checkTimer : Time -> Time -> Model -> ( Model, Cmd Model.Msg )
checkTimer t0 time model =
    if time - t0 > 60 * Time.second then
        ( backToInit model, Cmd.none )
    else
        ( model, Cmd.none )


receiveFinishPairing : String -> Time -> String -> String -> OtherSharedData -> Model -> ( Model, Cmd Model.Msg )
receiveFinishPairing token time otherToken otherId otherSync model =
    -- merge sync, send finish and go back to init. Also save here, as this after this there is no sync update
    if token == otherToken then
        let
            mergedSync =
                Data.Sync.merge time otherSync model.syncData

            newModel =
                backToInit model
                    |> (\m -> { m | syncData = mergedSync })
        in
            newModel
                |> withCmds [ finishPairing otherId token mergedSync, Ports.storeState (Data.Storage.encode newModel) ]
    else
        ( model, Cmd.none )


backToInit : Model -> Model
backToInit model =
    let
        state =
            model.protocolState

        newModel =
            { state | pairingState = Init }
                |> updateState model
                |> (\m -> { m | pairingDialogue = Views.Pairing.init })
    in
        newModel


{-| The pairing works as follows:
We start the process with initPairing. This sends our Id to the server, which replies with a random token.
We enter the token on another client and send it to the server. (pairWith)
The server sends both devices the id of the other device back. (PairedWith)
On receiving PairedWith, they send (FinishPairing token sync) to each other.
On receiving (FinishPairing token sync), check if the token matches, if yes pair.
-}
initPairing : String -> Model -> ( Model, Cmd Model.Msg )
initPairing uuid model =
    ( model
        |> updateProtocol (\s -> { s | pairingState = Init })
        |> (\m -> { m | pairingDialogue = Views.Pairing.getTockenClicked model.pairingDialogue })
    , (RemoteData.Http.postTask (apiUrl "/initPairing")
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ) ])
      )
        |> performWithTimestamp ReceiveToken
        |> Cmd.map Server
        |> Cmd.map protocolMsg
    )


pairWith : String -> Time -> Model -> ( Model, Cmd Model.Msg )
pairWith myId time model =
    model
        |> updateProtocol (\s -> { s | pairingState = WaitForPaired time model.pairingDialogue.inputToken })
        |> withCmds
            [ Http.post (apiUrl "/pairWith")
                (Http.jsonBody
                    (JE.object
                        [ ( "deviceId", JE.string myId )
                        , ( "token", JE.string model.pairingDialogue.inputToken )
                        ]
                    )
                )
                (JD.field "otherId" JD.string)
                |> Http.toTask
                |> Task.attempt PairedWith
                |> Cmd.map (protocolMsg << Server)
            ]


finishPairing : String -> String -> SyncData -> Cmd Model.Msg
finishPairing otherId token sync =
    sendMsgTo sync.id otherId "FinishPairing" [ ( "token", JE.string token ), ( "sync", Data.Sync.encode sync ) ]


syncToOthers : Model -> ( Model, Cmd Model.Msg )
syncToOthers model =
    let
        ( debounce, cmd ) =
            Debounce.push syncToOthersDebouncer () model.protocolState.debounce
    in
        ( updateProtocol (\s -> { s | debounce = debounce }) model, cmd )



-- Shares


{-| TODO!: this request should be sent multiple times, with a timeout
-}
requestShare : ( String, String ) -> SyncData -> Cmd Model.Msg
requestShare key sync =
    sendMsgToAll sync "RequestShare" [ ( "shareId", encodeTuple JE.string key ) ]


grantRequest : { key : ( String, String ), id : String } -> SyncData -> Cmd Model.Msg
grantRequest req sync =
    case Dict.get req.key sync.myShares of
        Just share ->
            sendMsgTo sync.id
                req.id
                "GrantedShareRequest"
                [ ( "share", SecretSharing.encodeShare share )
                , ( "shareId", encodeTuple JE.string req.key )
                ]

        Nothing ->
            Cmd.none



--


jsonToMsg : Value -> Time -> Msg
jsonToMsg msg time =
    case JD.decodeValue (serverResponseDecoder time) msg of
        Ok m ->
            m

        Err e ->
            Self (DecodeError e)


serverResponseDecoder : Time -> JD.Decoder Msg
serverResponseDecoder time =
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
                            |> JD.map (Authenticated id time)

                    "GotRemoved" ->
                        JD.succeed GotRemoved
                            |> JD.map (Authenticated id time)

                    "RequestShare" ->
                        JD.map RequestShare (JD.field "shareId" (decodeTuple JD.string))
                            |> JD.map (Authenticated id time)

                    "GrantedShareRequest" ->
                        JD.map2 GrantedShareRequest
                            (JD.field "shareId" (decodeTuple JD.string))
                            (JD.field "share" SecretSharing.shareDecoder)
                            |> JD.map (Authenticated id time)

                    "NeedsUpdate" ->
                        JD.map NeedsUpdate (JD.field "version" VClock.decoder)
                            |> JD.map (Authenticated id time)

                    "FinishPairing" ->
                        JD.map2 FinishPairing
                            (JD.field "token" JD.string)
                            (JD.field "sync" (Data.Sync.decoder id))
                            |> JD.map (Authenticated id time)

                    other ->
                        JD.fail ("no recognized type: " ++ other)
            )
