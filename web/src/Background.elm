module Background exposing (..)

import Dict exposing (Dict)
import Random.Pcg.Extended as RandomE
import SecretSharing


--

import Helper exposing (..)
import Ports
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.RequestPassword
import Data.Notifications as Notifications exposing (Notifications, Notification, ShareRequest, SiteEntry)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Storage
import Protocol.Api as Api
import Views.PasswordGenerator as PW
import Views.Pairing
import Model exposing (..)


-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        newModel =
            Model.init flags
    in
        newModel |> withCmds [ storeState newModel, Api.askForNewVersion newModel.syncData ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        OnStateRequest ->
            -- Since we send our state out on every update, we don't need to do anything here
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

        DismissNotification id ->
            model
                |> updateNotifications (Notifications.remove id)

        InsertSite siteName userName share requiredParts timestamp ->
            { model | syncData = Data.Sync.insertSite timestamp requiredParts siteName userName share model.syncData }
                |> Api.syncToOthers

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

        UserNameChanged n ->
            { model | newSiteEntry = (\e -> { e | userName = n }) model.newSiteEntry }
                |> noCmd

        DeletePassword key ->
            { model | syncData = Data.Sync.deletePassword key model.syncData }
                |> Api.syncToOthers

        ProtocolMsg msg ->
            Api.update model msg

        PairDeviceClicked ->
            { model | showPairingDialogue = not model.showPairingDialogue }
                |> noCmd

        GetTokenClicked ->
            model
                |> updatePairingDialogue Views.Pairing.getTockenClicked
                |> Api.initPairing model.uniqueIdentifyier

        UpdatePairing s ->
            { model | pairingDialogue = s }
                |> noCmd

        DoTokenSubmitted time ->
            model
                |> updatePairingDialogue Views.Pairing.tokenSubmitted
                |> Api.pairWith model.uniqueIdentifyier time

        TokenSubmitted ->
            model
                |> withCmds [ withTimestamp DoTokenSubmitted ]

        RemoveDevice uuid ->
            let
                ( sync, removeCmd ) =
                    Api.removeDevice uuid model.syncData
            in
                { model | syncData = sync }
                    |> Api.syncToOthers
                    |> addCmds [ removeCmd ]

        SetDeviceName newName ->
            let
                newSync =
                    Data.Sync.renameDevice newName model.syncData
            in
                { model | syncData = newSync }
                    |> Api.syncToOthers
                    |> addCmds [ Ports.setTitle ("NoPass (" ++ newName ++ ")") ]

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
                                    |> Api.requestShare key

                Nothing ->
                    -- TODO: should we really crash here?
                    Debug.crash "You somehow managed to press on RequestPassword for a site that we haven't saved!"

        GrantShareRequest id req ->
            model
                |> updateNotifications (Notifications.remove id)
                |> addCmds [ Api.grantRequest req model.syncData ]

        RejectShareRequest id ->
            -- TODO: inform other of reject?
            model
                |> updateNotifications (Notifications.remove id)

        ResetDevice ->
            Model.reset model
                |> withCmds [ Ports.resetStorage () ]

        SendOutAccountsFor site ->
            { model | currentSite = Just site }
                |> withCmds [ Ports.accountsForSite (Data.Sync.getAccountsForSite site model.syncData) ]

        AddSiteEntry { isSignUp, entry } ->
            if not isSignUp then
                model |> noCmd
            else
                case Data.Sync.getPasswordHashFor entry.site entry.login model.syncData of
                    Just hash ->
                        -- TODO: compare hashed password and compare with password hash to see if old or update
                        -- See problem on notes...
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

        FillForm config ->
            model |> withCmds [ Ports.fillForm config ]
    )
        |> (\( newModel, cmds ) -> ( newModel, Cmd.batch [ cmds, Ports.sendOutNewState (Model.encode newModel) ] ))


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


updateSeed : Model -> Model
updateSeed model =
    -- We need this to advance the seed so that after we created and accepted a password,
    -- we get a different series of random passswords
    { model
        | seed = RandomE.step (RandomE.int 1 42) model.seed |> Tuple.second
        , requirementsState = PW.nextPassword model.requirementsState
    }


storeState : Model -> Cmd Msg
storeState model =
    Ports.storeState (Data.Storage.encode model)



-- Subs


subs : Model -> Sub Msg
subs model =
    [ Api.connectPrivateSocket model.uniqueIdentifyier
    , Ports.onStateRequest (always OnStateRequest)
    , Ports.onReceiveMsg Model.decodeMsg
    , Ports.onRequestAccountsForSite SendOutAccountsFor
    , Ports.onAddSiteEntry AddSiteEntry
    ]
        |> Sub.batch
