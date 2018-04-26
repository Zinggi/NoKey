module Protocol.Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Task
import Time exposing (Time)
import Dict exposing (Dict)
import Set exposing (Set)
import Navigation


-- https://github.com/saschatimme/elm-phoenix

import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Debounce
import Crdt.VClock as VClock exposing (VClock)
import Timer


--

import Helper exposing (..)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Notifications as Notifications
import Data.RequestGroupPassword
import SecretSharing
import Views.Pairing
import Model exposing (Model, updateNotifications, updateProtocol, protocolMsg)
import Protocol.Data exposing (..)
import Ports
import Data.Storage
import Data exposing (..)


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
    -- TODO: make this configurable in a Settings UI
    -- TODO: change
    -- "localhost"
    {- etz upper -}
    -- "10.2.118.194"
    -- {- etz lower -}
    -- "10.2.122.231"
    -- {- hg lower -}
    -- "10.2.54.70"
    -- "floyogaarch.fritz.box"
    -- Server
    "nokey.xyz"
        -- |> (\ip -> pre ++ ip ++ ":4000" ++ path)
        |> (\ip -> pre ++ ip ++ path)


apiUrl : String -> String
apiUrl path =
    endPointUrl "https://" ("/api" ++ path)


socketUrl : String
socketUrl =
    endPointUrl "wss://" "/socket/websocket"



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


removeDevice : DeviceId -> SyncData -> ( SyncData, Cmd Model.Msg )
removeDevice uuid sync =
    ( Data.Sync.removeDevice uuid sync
    , informOfRemove uuid
    )


informOfRemove : DeviceId -> Cmd Model.Msg
informOfRemove otherId =
    sendMsgTo otherId "GotRemoved" []


sendMsgTo : DeviceId -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgTo otherId type_ content =
    Ports.getSignatureForMsg
        { msg = JE.object (( "type", JE.string type_ ) :: content)
        , otherId = otherId
        }


onSignedMsg : Sub Model.Msg
onSignedMsg =
    Ports.onSignedMsg
        (\{ data, signature, otherId } ->
            protocolMsg (Self (SendAuthenticatedMsgTo otherId data signature))
        )


sendAuthenticatedMsgTo : DeviceId -> DeviceId -> Value -> Value -> Cmd Model.Msg
sendAuthenticatedMsgTo myId otherId content signature =
    Http.post (apiUrl ("/sendMsgTo/" ++ otherId))
        (Http.jsonBody
            (JE.object
                [ ( "type", JE.string "Authenticated" )
                , ( "from", JE.string myId )
                , ( "data", content )
                , ( "signature", signature )
                ]
            )
        )
        (JD.succeed ())
        |> Http.send (always (protocolMsg (Self NoReply)))


sendNotAuthenticatedMsgTo : String -> String -> String -> List ( String, Value ) -> Cmd Model.Msg
sendNotAuthenticatedMsgTo myId otherId type_ content =
    Http.post (apiUrl ("/sendMsgTo/" ++ otherId))
        (Http.jsonBody (JE.object (( "type", JE.string type_ ) :: ( "from", JE.string myId ) :: content)))
        (JD.succeed ())
        |> Http.send (always (protocolMsg (Self NoReply)))


sendMsgToAll : SyncData -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgToAll sync type_ content =
    List.map (\id -> sendMsgTo id type_ content)
        (Data.Sync.knownOtherIds sync)
        |> Cmd.batch


sendMsgToGroup : List DeviceId -> String -> List ( String, Value ) -> Cmd Model.Msg
sendMsgToGroup otherIds type_ content =
    List.map (\id -> sendMsgTo id type_ content) otherIds |> Cmd.batch


askForNewVersion : SyncData -> Cmd Model.Msg
askForNewVersion sync =
    sendMsgToAll sync "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


askForNewVersionFrom : List String -> SyncData -> Cmd Model.Msg
askForNewVersionFrom otherIds sync =
    sendMsgToGroup otherIds "NeedsUpdate" [ ( "version", Data.Sync.encodeVersion sync ) ]


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
    sendMsgToGroup otherIds "SyncUpdate" [ ( "syncData", Data.Sync.encodeShared sync.shared ) ]


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

        -- _ =
        --     case msg of
        --         Self _ ->
        --             ()
        --         Unverified _ _ _ _ ->
        --             ()
        --         _ ->
        --             Debug.log ("\tgot msg:\n" ++ toString msg ++ "\n\tin state:\n" ++ toString state.pairingState) ()
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
                Debug.log "got token, but we were not expecting one" ( msg, state )
                    |> always ( model, Cmd.none )

            ( Server (PairedWith res), WaitForPaired t0 token ) ->
                (case res of
                    Ok otherId ->
                        ( { state | pairingState = WaitForFinished t0 token otherId }, finishPairing otherId token sync )

                    Err e ->
                        Debug.log "Got PairedWith, but with an error" e
                            |> always ( { state | pairingState = Init }, Cmd.none )
                )
                    |> toModel model
                    |> mapModel (\m -> { m | pairingDialogue = Views.Pairing.pairingCompleted res m.pairingDialogue })

            ( Server (PairedWith _), _ ) ->
                Debug.log "got PairedWith, but we were not expecting it now" ( msg, state )
                    |> always ( model, Cmd.none )

            --
            ( FinishPairing otherId time otherToken otherSync, WaitForPaired t0 token ) ->
                receiveFinishPairing token time otherToken otherId otherSync model

            ( FinishPairing otherId time otherToken otherSync, WaitForFinished t0 token _ ) ->
                receiveFinishPairing token time otherToken otherId otherSync model

            ( FinishPairing otherId time otherToken otherSync, Init ) ->
                -- This happens when we are already paired, beacause finishPairing is sent multiple times
                ( model, Cmd.none )

            -- Independant of current state
            ( Unverified otherId time data signature, _ ) ->
                case Data.Sync.getSigningKeyOf otherId model.syncData of
                    Just key ->
                        model
                            |> withCmds
                                [ Ports.verifyAuthenticity
                                    { data = data, signature = signature, key = key, from = otherId, time = time }
                                ]

                    Nothing ->
                        let
                            _ =
                                Debug.log "Received a msg from an unknown source (id, time, data, signature)" ( otherId, time, data, signature )
                        in
                            model |> noCmd

            --
            ( Authenticated otherId time (RequestShares id keys), _ ) ->
                -- On a share request, filter out the ones where we have a share and ask the user
                -- to unlock those.
                -- If we already have a pending request from the same device, merge the request
                -- The ones we don't have can be immediately rejected.
                let
                    groupIds =
                        Data.Sync.groupIdsWithShare keys model.syncData

                    ( nId, notifications ) =
                        Notifications.newShareRequestWithId id otherId groupIds model.notifications
                in
                    if List.isEmpty groupIds then
                        model
                            |> withCmds [ rejectShareRequest otherId (Set.singleton id) ]
                    else
                        model
                            |> updateNotifications (always notifications)
                            |> andThenUpdate (startTimer (ShareRequest nId) (60 * Time.second))

            ( Authenticated otherId time (GrantedShareRequest ids shares), _ ) ->
                model
                    |> withCmds [ Ports.decryptRequestedShares { ids = Set.toList ids, shares = shares, time = time, otherId = otherId } ]

            ( Authenticated otherId time (RejectShareRequest id), _ ) ->
                -- stop asking those that reject our request
                { state | collectShares = stopAskingDevice id otherId state.collectShares }
                    |> updateState model
                    |> noCmd

            ( Authenticated otherId time (SyncUpdate otherSync), _ ) ->
                let
                    ( debounce, cmd ) =
                        Debounce.push syncToOthersDebouncer () state.debounce

                    ( newSync, syncCmd ) =
                        Data.Sync.merge decryptMyShares time otherSync sync
                in
                    { state | debounce = debounce }
                        |> updateState model
                        |> (\m -> { m | syncData = newSync })
                        |> withCmds [ cmd, syncCmd ]

            ( Authenticated otherId time GotRemoved, _ ) ->
                { model | syncData = Data.Sync.gotRemoved model.syncData }
                    |> syncToOthers

            ( Authenticated otherId time (NeedsUpdate version), _ ) ->
                { model | syncData = Data.Sync.receiveVersion otherId version model.syncData }
                    |> syncToOthers

            --
            ( Self (JoinedChannel v), _ ) ->
                -- let
                --     _ =
                --         Debug.log "(re)join channel" v
                -- in
                model
                    |> withCmds [ askForNewVersion sync ]

            ( Self (NewMsg v), _ ) ->
                model
                    |> withCmds [ withTimestamp (jsonToMsg v >> protocolMsg) ]

            ( Self (SendAuthenticatedMsgTo otherId data signature), _ ) ->
                model |> withCmds [ sendAuthenticatedMsgTo model.syncData.id otherId data signature ]

            ( Self (SyncToOthers debounceMsg), _ ) ->
                -- delay the sync update to others, as we might get multiple updates in a short time, so wait until it settled.
                -- Since we always call this when we got a new state, we also store our state here
                let
                    ( newSync, cmdToDebounce ) =
                        doSyncToOthers sync

                    ( debounce, cmd ) =
                        Debounce.update
                            syncToOthersDebouncer
                            (Debounce.takeLast (always cmdToDebounce))
                            debounceMsg
                            state.debounce

                    newModel =
                        { state | debounce = debounce }
                            |> updateState model
                            |> (\m -> { m | syncData = newSync })
                in
                    newModel
                        |> withCmds [ cmd, Ports.storeState (Data.Storage.encode newModel) ]

            ( Self (DecodeError e), _ ) ->
                -- TODO: Display msg to user, instructing him to update his or the other client
                Debug.log "faild to decode msg" e
                    |> always ( model, Cmd.none )

            ( Self (FailedToVerifyAuthenticityOf otherId time data), _ ) ->
                Debug.log "faild to verify authenticity of (otherId, time, msg)" ( otherId, time, data )
                    |> always ( model, Cmd.none )

            ( Self (Timer m), _ ) ->
                let
                    ( newTimer, cmd ) =
                        Timer.update timerConfig m state.timer
                in
                    model
                        |> updateProtocol (\p -> { p | timer = newTimer })
                        |> withCmds [ cmd ]

            ( Self (OnInterval id time), _ ) ->
                case id of
                    Pairing ->
                        case state.pairingState of
                            WaitForFinished _ token otherId ->
                                model |> withCmds [ finishPairing otherId token sync ]

                            _ ->
                                model |> noCmd

                    CollectShares cId ->
                        case state.collectShares of
                            WaitForShares dict ->
                                case Dict.get cId dict of
                                    Just ( devs, groups ) ->
                                        ( model, doRequestShares (Set.toList devs) cId groups )

                                    Nothing ->
                                        model |> noCmd

                            Start ->
                                model |> noCmd

                    ShareRequest _ ->
                        model |> noCmd

            ( Self (OnFinishTimer id time), _ ) ->
                case id of
                    Pairing ->
                        backToInit model
                            |> noCmd

                    CollectShares cId ->
                        let
                            removeAll m =
                                { state | collectShares = Start }
                                    |> updateState
                                        { m | syncData = Data.Sync.updateGroupPasswordRequest Data.RequestGroupPassword.removeWaiting m.syncData }
                                    |> noCmd
                        in
                            case state.collectShares of
                                WaitForShares dict ->
                                    case Dict.get cId dict of
                                        Just ( devs, groups ) ->
                                            { state | collectShares = removeFromWaitForShares cId state.collectShares }
                                                |> updateState
                                                    { model
                                                        | syncData =
                                                            Data.Sync.updateGroupPasswordRequest
                                                                (Data.RequestGroupPassword.removeWaitingGroups groups)
                                                                model.syncData
                                                    }
                                                |> noCmd

                                        Nothing ->
                                            removeAll model

                                Start ->
                                    removeAll model

                    ShareRequest nId ->
                        updateNotifications (Notifications.remove nId) model

            ( Self NoReply, _ ) ->
                ( model, Cmd.none )
        )


removeFromWaitForShares : String -> CollectSharesState -> CollectSharesState
removeFromWaitForShares id state =
    case state of
        WaitForShares dict ->
            let
                newDict =
                    Dict.remove id dict
            in
                if Dict.isEmpty newDict then
                    Start
                else
                    WaitForShares newDict

        Start ->
            Start


grantedShareRequest : Ports.DidDecryptRequestedSharesT -> Model -> ( Model, Cmd Model.Msg )
grantedShareRequest { shares, time, otherId, ids } model =
    case JD.decodeValue (JD.list (decodeTuple2 groupIdDecoder SecretSharing.shareDecoder)) shares of
        Ok decodedShares ->
            let
                state =
                    model.protocolState

                ( newSync, mayForm, cmd ) =
                    Data.Sync.addShares encryptNewShares time decodedShares model.syncData

                newModel =
                    -- stop asking the ones that already gave us a share
                    { state | collectShares = stopAskingDevice (Set.fromList ids) otherId state.collectShares }
                        |> updateState { model | syncData = newSync }
            in
                case mayForm of
                    Just formData ->
                        -- if done and fillForm is set, call port to fill the form
                        newModel
                            |> withCmds [ Task.perform (\_ -> Model.FillForm formData) (Task.succeed ()), cmd ]
                            |> andThenUpdate syncToOthers

                    Nothing ->
                        newModel
                            |> syncToOthers
                            |> addCmds [ cmd ]

        Err e ->
            Debug.log ("Failed to decode received shares:\n" ++ e) (model |> noCmd)


receiveFinishPairing : String -> Time -> String -> String -> OtherSharedData -> Model -> ( Model, Cmd Model.Msg )
receiveFinishPairing token time otherToken otherId otherSync model =
    -- merge sync, send finish and go back to init. Also save here, as after this there is no sync update
    if token == otherToken then
        let
            ( mergedSync, decryptCmd ) =
                Data.Sync.merge decryptMyShares time otherSync model.syncData

            -- Create shares for the new device for all currently unlocked groups
            ( newSync, encryptCmd ) =
                Data.Sync.createNewSharesIfPossible encryptNewShares time mergedSync

            newModel =
                backToInit model
                    |> (\m -> { m | syncData = newSync })
        in
            newModel
                |> withCmds
                    [ finishPairing otherId token newSync
                    , Ports.storeState (Data.Storage.encode newModel)
                    , decryptCmd
                    , encryptCmd
                    , Navigation.back 1
                    ]
                |> andThenUpdate syncToOthers
    else
        ( model, Cmd.none )


encryptNewShares : Time -> GroupId -> List ( DeviceId, ( Value, Value ) ) -> Cmd msg
encryptNewShares time groupId shares =
    if List.isEmpty shares then
        Cmd.none
    else
        Ports.encryptNewShares
            { time = time
            , groupId = groupId
            , shares = shares
            }


decryptMyShares : Dict GroupId Value -> Cmd Model.Msg
decryptMyShares shares =
    Ports.decryptMyShares (Dict.toList shares)


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
    , RemoteData.Http.postTask (apiUrl "/initPairing")
        (JD.at [ "token" ] JD.string)
        (JE.object [ ( "deviceId", JE.string uuid ) ])
        |> performWithTimestamp ReceiveToken
        |> Cmd.map Server
        |> Cmd.map protocolMsg
    )
        |> andThenUpdate (startTimer Pairing (60 * Time.second))


timerConfig : Timer.Config TimerId Model.Msg
timerConfig =
    { onInterval = \a b -> protocolMsg (Self (OnInterval a b))
    , onFinish = \a b -> protocolMsg (Self (OnFinishTimer a b))
    , toMsg = protocolMsg << Self << Timer
    , frequency = 5 * Time.second
    }


startTimer : TimerId -> Time -> Model -> ( Model, Cmd Model.Msg )
startTimer id timeout model =
    let
        ( newT, cmd ) =
            Timer.startTimer timerConfig id timeout model.protocolState.timer
    in
        ( model |> updateProtocol (\p -> { p | timer = newT }), cmd )


pairWith : String -> Time -> Model -> ( Model, Cmd Model.Msg )
pairWith myId time model =
    let
        cleanToken =
            Helper.cleanString model.pairingDialogue.inputToken |> Helper.replaceString " " "-"
    in
        model
            |> updateProtocol (\s -> { s | pairingState = WaitForPaired time cleanToken })
            |> withCmds
                [ Http.post (apiUrl "/pairWith")
                    (Http.jsonBody
                        (JE.object
                            [ ( "deviceId", JE.string myId )

                            -- Remove unnecessary whitespace from token and replace spaces with dashes
                            , ( "token", JE.string cleanToken )
                            ]
                        )
                    )
                    (JD.field "otherId" JD.string)
                    |> Http.toTask
                    |> Task.attempt PairedWith
                    |> Cmd.map (protocolMsg << Server)
                ]
            |> andThenUpdate (startTimer Pairing (60 * Time.second))


finishPairing : String -> String -> SyncData -> Cmd Model.Msg
finishPairing otherId token sync =
    sendNotAuthenticatedMsgTo sync.id otherId "FinishPairing" [ ( "token", JE.string token ), ( "sync", Data.Sync.encodeShared sync.shared ) ]


syncToOthers : Model -> ( Model, Cmd Model.Msg )
syncToOthers model =
    let
        ( debounce, cmd ) =
            Debounce.push syncToOthersDebouncer () model.protocolState.debounce
    in
        ( updateProtocol (\s -> { s | debounce = debounce }) model, cmd )



-- Shares


{-| Request some shares from all devices we know.
-}
requestShares : List GroupId -> Model -> ( Model, Cmd Model.Msg )
requestShares keys model =
    let
        ( id, newModel ) =
            Model.getUniqueId model

        devs =
            Data.Sync.knownOtherIds newModel.syncData
    in
        newModel
            |> waitForShares id keys
            |> startTimer (CollectShares id) (60 * Time.second)
            |> addCmds [ doRequestShares devs id keys ]


{-| keep track of who already answered our request
-}
waitForShares : String -> List GroupId -> Model -> Model
waitForShares reqId keys model =
    let
        devices =
            Data.Sync.knownOtherIds model.syncData |> Set.fromList
    in
        updateProtocol
            (\s ->
                case s.collectShares of
                    WaitForShares dict ->
                        { s | collectShares = WaitForShares (Dict.insert reqId ( devices, keys ) dict) }

                    Start ->
                        { s | collectShares = WaitForShares (Dict.singleton reqId ( devices, keys )) }
            )
            model


{-| Call this once we got a reply from someone
-}
stopAskingDevice : Set String -> DeviceId -> CollectSharesState -> CollectSharesState
stopAskingDevice ids otherId collectShares =
    case collectShares of
        WaitForShares dict ->
            WaitForShares <|
                Set.foldl (\id -> Dict.update id (Maybe.map (\( devs, gs ) -> ( Set.remove otherId devs, gs ))))
                    dict
                    ids

        Start ->
            Start


{-| Each request has a unique request id, this way we can easily keep track of them
-}
doRequestShares : List DeviceId -> String -> List GroupId -> Cmd Model.Msg
doRequestShares otherIds id keys =
    sendMsgToGroup
        otherIds
        "RequestShares"
        [ ( "groupIds", JE.list (List.map encodeGroupId keys) ), ( "requestId", JE.string id ) ]


rejectShareRequest : DeviceId -> Set String -> Cmd Model.Msg
rejectShareRequest deviceId reqIds =
    sendMsgTo deviceId "RejectShareRequest" [ ( "requestIds", encodeSet JE.string reqIds ) ]


grantRequest : Notifications.ShareRequest -> SyncData -> Cmd Model.Msg
grantRequest req sync =
    case Data.Sync.getShares req.keys sync of
        [] ->
            Cmd.none

        shares ->
            case Data.Sync.getEncryptionKeyOf req.deviceId sync of
                Just key ->
                    Ports.encryptShares
                        { deviceId = req.deviceId
                        , shares = List.map (\( g, s ) -> ( g, SecretSharing.encodeShare s )) shares
                        , publicKey = key
                        , reqIds = encodeSet JE.string req.reqIds
                        }

                Nothing ->
                    Cmd.none


sendGrantedRequest : Ports.DidEncryptSharesT -> Cmd Model.Msg
sendGrantedRequest { deviceId, encryptedShares, reqIds } =
    sendMsgTo deviceId
        "GrantedShareRequest"
        [ ( "shares", encryptedShares )
        , ( "ids", reqIds )
        ]



--


jsonToMsg : Value -> Time -> Msg
jsonToMsg msg time =
    case JD.decodeValue (serverResponseDecoder time) msg of
        Ok m ->
            m

        Err e ->
            Self (DecodeError e)


onAuthenticatedMsg : Sub Model.Msg
onAuthenticatedMsg =
    Ports.onAuthenticatedMsg
        (\{ time, from, data, isAuthentic } ->
            if isAuthentic then
                case JD.decodeValue (authenticatedMsgDecoder from time) data of
                    Ok m ->
                        m

                    Err e ->
                        Self (DecodeError e)
            else
                Self (FailedToVerifyAuthenticityOf from time data)
        )
        |> Sub.map protocolMsg


authenticatedMsgDecoder : DeviceId -> Time -> Decoder Msg
authenticatedMsgDecoder id time =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "SyncUpdate" ->
                        JD.map SyncUpdate (JD.field "syncData" (Data.Sync.otherSharedDecoder id))

                    "GotRemoved" ->
                        JD.succeed GotRemoved

                    "RequestShares" ->
                        JD.map2 RequestShares
                            (JD.field "requestId" JD.string)
                            (JD.field "groupIds" (JD.list groupIdDecoder))

                    "RejectShareRequest" ->
                        JD.map RejectShareRequest (JD.field "requestIds" (decodeSet JD.string))

                    "GrantedShareRequest" ->
                        JD.map2 GrantedShareRequest
                            (JD.field "ids" (decodeSet JD.string))
                            (JD.field "shares" JD.value)

                    "NeedsUpdate" ->
                        JD.map NeedsUpdate (JD.field "version" VClock.decoder)

                    other ->
                        JD.fail ("no recognized type: " ++ other)
            )
        |> JD.map (Authenticated id time)


serverResponseDecoder : Time -> JD.Decoder Msg
serverResponseDecoder time =
    JD.map2 (,)
        (JD.field "from" JD.string)
        (JD.field "type" JD.string)
        |> JD.andThen
            (\( id, t ) ->
                case t of
                    "PairedWith" ->
                        JD.succeed (PairedWith (Ok id))
                            |> JD.map Server

                    "FinishPairing" ->
                        JD.map2 (FinishPairing id time)
                            (JD.field "token" JD.string)
                            (JD.field "sync" (Data.Sync.otherSharedDecoder id))

                    "Authenticated" ->
                        JD.map2 (Unverified id time)
                            (JD.field "data" JD.value)
                            (JD.field "signature" JD.value)

                    other ->
                        JD.fail ("no recognized type: " ++ other)
            )
