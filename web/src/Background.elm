module Background exposing (..)

import Date
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Http
import Json.Encode as JE exposing (Value)
import Random.Pcg as Random exposing (Generator, Seed)
import Random.Pcg.Extended as RandomE
import RemoteData exposing (WebData)
import SecretSharing
import Time exposing (Time)


--

import Helper exposing (..)
import Ports
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.RequestPassword
import Data.Notifications as Notifications exposing (Notifications, Notification, ShareRequest, SiteEntry)
import Views.Notifications
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Storage
import Protocol.Api as Api
import Views.PasswordGenerator as PW
import Views.Pairing
import PortUtils


-- Model


decodeModel : Value -> Result String Model
decodeModel value =
    Ok (PortUtils.fromJs value)


encodeModel : Model -> Value
encodeModel model =
    PortUtils.toJs model


encodeMsg : Msg -> Value
encodeMsg msg =
    PortUtils.toJs msg


decodeMsg : Value -> Msg
decodeMsg value =
    PortUtils.fromJs value


type alias Model =
    { newSiteEntry : PasswordMetaData
    , expandSiteEntry : Bool
    , requirementsState : PW.State
    , pairingDialogue : Views.Pairing.State
    , showPairingDialogue : Bool
    , seed : RandomE.Seed
    , sitesState : Data.RequestPassword.State
    , debounce : Debounce ()
    , notifications : Notifications
    , notificationsView : Views.Notifications.State

    -- Keep the current site, to provide site specific actions
    , currentSite : Maybe String

    -- These ones should be serialized:
    , uniqueIdentifyier : String

    -- CRDT for synchronisation
    , syncData : SyncData
    }


type alias Flags =
    { initialSeed : ( Int, List Int )
    , storedState : Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newModel =
            initModel flags
    in
        -- TODO: ask other devices for updates, e.g. only send our version (unless we know we had an offline change)
        newModel |> withCmds [ storeState newModel ]


initModel : Flags -> Model
initModel { initialSeed, storedState } =
    let
        ( base, ext ) =
            initialSeed

        ( uuid, seed2 ) =
            -- TODO: replace with pcg-extended
            -- TODO:
            -- adapt the UUID package to use pcg-extended
            -- see:
            --  https://github.com/danyx23/elm-uuid/issues/10
            Random.step randomUUID (Random.initialSeed base)

        ( indepSeed, seed3 ) =
            Random.step Random.independentSeed seed2

        makeInit mayId maySync =
            { newSiteEntry = Data.PasswordMeta.default
            , expandSiteEntry = False
            , requirementsState = PW.init (RandomE.initialSeed base ext)
            , seed = RandomE.initialSeed base ext
            , debounce = Debounce.init
            , uniqueIdentifyier = Maybe.withDefault uuid mayId
            , syncData = Maybe.withDefault (Data.Sync.init indepSeed uuid) maySync
            , pairingDialogue = Views.Pairing.init
            , showPairingDialogue = True
            , notifications = Notifications.init
            , notificationsView = Views.Notifications.init
            , sitesState = Data.RequestPassword.init
            , currentSite = Nothing
            }
    in
        case Data.Storage.decode storedState of
            Ok { syncData, uniqueIdentifyier } ->
                makeInit (Just uniqueIdentifyier) (Just syncData)

            Err err ->
                let
                    _ =
                        Debug.log "couldn't decode state" err
                in
                    makeInit Nothing Nothing


resetModel : Model -> Model
resetModel model =
    let
        int32 =
            (RandomE.int RandomE.minInt RandomE.maxInt)

        ( initSeed, _ ) =
            RandomE.step (RandomE.map2 (,) int32 (RandomE.list 8 int32)) model.seed
    in
        initModel { initialSeed = initSeed, storedState = JE.null }



-- Update


type Msg
    = AddPassword String
    | SiteNameChanged String
    | PasswordLengthChanged Int
    | SecurityLevelChanged Int
    | NewPasswordRequirements PW.State
    | UserNameChanged String
    | ReceiveMessage JE.Value
    | DecodeReceivedMessage JE.Value Time
    | ReceiveToken (WebData String)
    | PairDeviceClicked
    | GetTokenClicked
    | UpdatePairing Views.Pairing.State
    | TokenSubmitted
    | PairedWith (Result Http.Error ( String, OtherSharedData, Time ))
    | JoinChannel JE.Value
    | RemoveDevice String
    | SetDeviceName String
    | SyncToOthers Debounce.Msg
    | InsertSite String String (Dict String SecretSharing.Share) Int Time
    | RequestPasswordPressed ( String, String ) Bool
    | GrantShareRequest Notifications.Id ShareRequest
    | RejectShareRequest Notifications.Id
    | ResetDevice
    | SendOutAccountsFor String
    | AddSiteEntry SiteEntry
    | UpdateNotifications Views.Notifications.State
    | SaveEntry Notifications.Id SiteEntry
    | DismissNotification Notifications.Id
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            model |> noCmd

        AddPassword pw ->
            let
                { userName, siteName } =
                    model.newSiteEntry
            in
                saveEntry { login = userName, site = siteName, password = pw, securityLevel = model.newSiteEntry.securityLevel } model
                    |> mapModel updateSeed

        SaveEntry id entry ->
            saveEntry entry model
                |> andThenUpdate (updateNotifications (Notifications.remove id))
                |> mapModel updateSeed

        DismissNotification id ->
            model
                |> updateNotifications (Notifications.remove id)

        InsertSite siteName userName share requiredParts timestamp ->
            { model | syncData = Data.Sync.insertSite timestamp requiredParts siteName userName share model.syncData }
                |> syncToOthers

        SiteNameChanged s ->
            { model
                | newSiteEntry = (\e -> { e | siteName = s }) model.newSiteEntry
                , expandSiteEntry = not <| String.isEmpty s
            }
                |> noCmd

        SecurityLevelChanged n ->
            { model | newSiteEntry = (\e -> { e | securityLevel = n }) model.newSiteEntry }
                |> noCmd

        NewPasswordRequirements state ->
            { model | requirementsState = state }
                |> noCmd

        PasswordLengthChanged l ->
            { model | newSiteEntry = (\e -> { e | length = l }) model.newSiteEntry }
                |> updateSeed
                |> noCmd

        UserNameChanged n ->
            { model | newSiteEntry = (\e -> { e | userName = n }) model.newSiteEntry }
                |> noCmd

        ReceiveMessage msg ->
            model |> withCmds [ withTimestamp (DecodeReceivedMessage msg) ]

        DecodeReceivedMessage msg timestamp ->
            -- TODO: Include authenticity to messages and reject not authentic messages
            case Api.decodeServerResponse msg of
                Ok ( id, apiMsg ) ->
                    let
                        _ =
                            Debug.log ("at:   " ++ toString (Date.fromTime timestamp) ++ "\nfrom: " ++ id ++ "\n\n")
                                apiMsg
                    in
                        case apiMsg of
                            Api.PairedWith syncData ->
                                -- TODO: only accept a PairedWith message if we are expecting one.
                                -- Otherwise anyone could pair with us, as long as they know our id
                                { model | pairingDialogue = Views.Pairing.pairingCompleted (Ok id) model.pairingDialogue }
                                    |> pairedWith timestamp id syncData

                            other ->
                                if Data.Sync.isKnownId id model.syncData then
                                    case other of
                                        Api.SyncUpdate syncData ->
                                            syncUpdate timestamp syncData model

                                        Api.NeedsUpdate version ->
                                            { model | syncData = Data.Sync.receiveVersion id version model.syncData }
                                                |> syncToOthers

                                        Api.GotRemoved ->
                                            { model | syncData = Data.Sync.gotRemoved model.syncData } |> noCmd

                                        Api.RequestShare key ->
                                            model
                                                |> updateNotifications (Notifications.newShareRequest id key)

                                        Api.GrantedShareRequest key share ->
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
                                                        -- TODO: if done and fillForm is set, call port to fill the form
                                                        newModel |> withCmds [ Ports.fillForm { login = login, site = site, password = pw } ]

                                                    _ ->
                                                        newModel |> noCmd

                                        Api.PairedWith _ ->
                                            -- TODO: refactor, split Api messages into Authenticated / Anonymous messages
                                            Debug.crash "This can't happen, the branch is already covered by the surrounding case statement"
                                else
                                    let
                                        _ =
                                            Debug.log ("unknown id: " ++ id)
                                    in
                                        model |> noCmd

                Err e ->
                    let
                        _ =
                            Debug.log "parse msg failed:" e
                    in
                        model |> noCmd

        ReceiveToken token ->
            ( { model | pairingDialogue = Views.Pairing.receivedToken token model.pairingDialogue }, Cmd.none )

        PairDeviceClicked ->
            { model | showPairingDialogue = not model.showPairingDialogue }
                |> noCmd

        GetTokenClicked ->
            model
                |> (\m -> { m | pairingDialogue = Views.Pairing.getTockenClicked model.pairingDialogue })
                |> withCmds [ Api.initPairing ReceiveToken model.uniqueIdentifyier model.syncData ]

        UpdatePairing s ->
            { model | pairingDialogue = s }
                |> noCmd

        TokenSubmitted ->
            model
                |> (\m -> { m | pairingDialogue = Views.Pairing.tokenSubmitted m.pairingDialogue })
                |> withCmds [ Api.pairWith PairedWith model.uniqueIdentifyier model.pairingDialogue.inputToken model.syncData ]

        PairedWith res ->
            case res of
                Ok ( id, syncData, timestamp ) ->
                    model
                        |> (\m -> { m | pairingDialogue = Views.Pairing.pairingCompleted (Ok id) m.pairingDialogue })
                        |> pairedWith timestamp id syncData

                Err e ->
                    model
                        |> (\m -> { m | pairingDialogue = Views.Pairing.pairingCompleted (Err e) m.pairingDialogue })
                        |> noCmd

        JoinChannel v ->
            let
                _ =
                    Debug.log "(re)join channel" ()
            in
                model |> withCmds [ Api.askForNewVersion NoOp model.syncData ]

        RemoveDevice uuid ->
            let
                ( sync, removeCmd ) =
                    Api.removeDevice NoOp uuid model.syncData
            in
                { model | syncData = sync }
                    |> syncToOthers
                    |> addCmds [ removeCmd ]

        SetDeviceName newName ->
            let
                newSync =
                    -- do not sync immediately to reduce #of messages.
                    Data.Sync.renameDevice newName model.syncData
            in
                { model | syncData = newSync }
                    |> syncToOthers
                    |> addCmds [ Ports.setTitle ("NoPass (" ++ newName ++ ")") ]

        SyncToOthers msg ->
            -- delay the sync update to others, as we might get multiple updates in a short time, so wait until it settled.
            let
                ( newSync, cmdToDebounce ) =
                    Api.syncToOthers NoOp model.syncData

                ( debounce, cmd ) =
                    Debounce.update
                        syncToOthersDebouncer
                        (Debounce.takeLast (always cmdToDebounce))
                        msg
                        model.debounce

                newModel =
                    { model | debounce = debounce, syncData = newSync }
            in
                newModel
                    |> withCmds [ cmd, storeState newModel ]

        RequestPasswordPressed key fillForm ->
            case Data.Sync.getSavedSite key model.syncData of
                Just ( requiredParts, maybeMyShare ) ->
                    let
                        newSitesState =
                            Data.RequestPassword.waitFor key fillForm requiredParts maybeMyShare model.sitesState

                        newModel =
                            { model | sitesState = newSitesState }

                        ( site, login ) =
                            key
                    in
                        case Data.RequestPassword.getStatus key newSitesState of
                            Data.RequestPassword.Done True pw ->
                                newModel
                                    |> withCmds [ Ports.fillForm { password = pw, site = site, login = login } ]

                            _ ->
                                newModel
                                    |> withCmds
                                        [ -- TODO: this request should be sent multiple times, with a timeout
                                          Api.requestShare NoOp key model.syncData
                                        ]

                Nothing ->
                    -- TODO:
                    Debug.crash "You somehow managed to press on RequestPassword for a site that we haven't saved!"

        GrantShareRequest id req ->
            model
                |> updateNotifications (Notifications.remove id)
                |> addCmds [ Api.grantRequest NoOp req model.syncData ]

        RejectShareRequest id ->
            -- TODO: inform other of reject?
            model
                |> updateNotifications (Notifications.remove id)

        ResetDevice ->
            resetModel model
                |> withCmds [ Ports.resetStorage () ]

        SendOutAccountsFor site ->
            { model | currentSite = Just site }
                |> withCmds [ Ports.accountsForSite (Data.Sync.getAccountsForSite site model.syncData) ]

        AddSiteEntry entry ->
            case Data.Sync.getPasswordHashFor entry.site entry.login model.syncData of
                Just hash ->
                    -- TODO: compare hashed password and compare with password hash to see if old or update
                    if {- TODO: hash == pwHash entry.password -} False then
                        -- Ignore, since we already have that password
                        model |> noCmd
                    else
                        -- Update existing entry
                        model |> updateNotifications (Notifications.newSiteEntry entry False)

                Nothing ->
                    model
                        |> updateNotifications (Notifications.newSiteEntry entry True)

        UpdateNotifications n ->
            { model | notificationsView = n } |> noCmd
    )
        |> (\( newModel, cmds ) -> ( newModel, Cmd.batch [ cmds, Ports.sendOutNewState (encodeModel newModel) ] ))


saveEntry : SiteEntry -> Model -> ( Model, Cmd Msg )
saveEntry entry model =
    let
        ( shares, seed2 ) =
            SecretSharing.splitString
                ( entry.securityLevel
                , Data.Sync.knownIds model.syncData |> List.length
                )
                entry.password
                model.seed

        share =
            List.map2
                (\id share ->
                    ( id, share )
                )
                (Data.Sync.knownIds model.syncData)
                shares
                |> Dict.fromList
    in
        { model
            | newSiteEntry = Data.PasswordMeta.reset model.newSiteEntry
            , expandSiteEntry = False
            , seed = seed2
        }
            |> withCmds [ withTimestamp (InsertSite entry.site entry.login share entry.securityLevel) ]


updateNotifications : (Notifications -> Notifications) -> Model -> ( Model, Cmd Msg )
updateNotifications f model =
    let
        newNot =
            f model.notifications
    in
        { model | notifications = newNot }
            |> withCmds [ Ports.notificationCount (Notifications.count newNot) ]


updateSeed : Model -> Model
updateSeed model =
    -- TODO: what is this used for? is it actually needed?
    { model
        | seed = RandomE.step (RandomE.int 1 42) model.seed |> Tuple.second
        , requirementsState = PW.nextPassword model.requirementsState
    }


syncToOthersDebouncer : Debounce.Config Msg
syncToOthersDebouncer =
    { strategy = Debounce.soon (1 * Time.second)
    , transform = SyncToOthers
    }


syncToOthers : Model -> ( Model, Cmd Msg )
syncToOthers model =
    let
        ( debounce, cmd ) =
            Debounce.push syncToOthersDebouncer () model.debounce
    in
        ( { model | debounce = debounce }, cmd )


pairedWith : Time -> String -> OtherSharedData -> Model -> ( Model, Cmd Msg )
pairedWith timestamp id syncData model =
    -- TODO: delete? same as sync update
    model
        |> syncUpdate timestamp syncData


syncUpdate : Time -> OtherSharedData -> Model -> ( Model, Cmd Msg )
syncUpdate timestamp syncData model =
    let
        newSync =
            Data.Sync.merge timestamp syncData model.syncData

        ( debounce, cmd ) =
            Debounce.push syncToOthersDebouncer () model.debounce
    in
        ( { model | syncData = newSync, debounce = debounce }
        , cmd
        )



-- Cmds


storeState : Model -> Cmd Msg
storeState model =
    Ports.storeState (Data.Storage.encode model)



-- Subs


subs : Model -> Sub Msg
subs model =
    [ Api.connectPrivateSocket ReceiveMessage JoinChannel model.uniqueIdentifyier
    , Ports.onStateRequest (always NoOp)
    , Ports.onReceiveMsg decodeMsg
    , Ports.onRequestAccountsForSite SendOutAccountsFor
    , Ports.onAddSiteEntry AddSiteEntry
    ]
        |> Sub.batch
