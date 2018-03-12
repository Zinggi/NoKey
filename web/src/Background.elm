module Background exposing (..)

import Dict exposing (Dict)
import Random.Pcg.Extended as RandomE
import SecretSharing


--

import Helper exposing (..)
import Ports
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.RequestGroupPassword
import Data.Notifications as Notifications exposing (Notifications, Notification, ShareRequest, SiteEntry)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Storage
import Protocol.Api as Api
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Passwords
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

        AddPassword groupId pw ->
            let
                { userName, siteName, securityLevel } =
                    model.newSiteEntry
            in
                saveEntry groupId { login = userName, site = siteName, password = pw, securityLevel = securityLevel } model
                    |> mapModel updateSeed

        SaveEntry id groupId entry ->
            saveEntry groupId entry model
                |> andThenUpdate (updateNotifications (Notifications.remove id))
                |> addCmds [ Ports.closePopup () ]

        DismissNotification id ->
            model
                |> updateNotifications (Notifications.remove id)
                |> addCmds [ Ports.closePopup () ]

        InsertSite accountId groupId password timestamp ->
            let
                ( newSync, newSeed, shouldAsk ) =
                    Data.Sync.insertSite timestamp model.seed accountId groupId password model.syncData
            in
                { model | syncData = newSync, seed = newSeed }
                    |> Api.syncToOthers
                    |> andThenUpdateIf shouldAsk (Api.requestShare groupId)

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

        TogglePassword accountId ->
            { model | syncData = Data.Sync.togglePassword accountId model.syncData }
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
                    |> addCmds
                        [ Ports.setTitle
                            (if newName == "" then
                                "NoKey"
                             else
                                "NoKey (" ++ newName ++ ")"
                            )
                        ]

        RequestPasswordPressed key mayFill ->
            let
                ( newSync, mayForm ) =
                    Data.Sync.requestPasswordPressed key mayFill model.syncData

                newModel =
                    { model | syncData = newSync }
            in
                case mayForm of
                    Just formData ->
                        newModel
                            |> withCmds [ Ports.fillForm formData ]

                    Nothing ->
                        newModel
                            |> Api.requestShare key

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
            -- TODO: type can be either SignUp | LogIn | UpdateCredentials
            -- LogIn can be ignored if we have already an entry for it
            if not isSignUp && Data.Sync.hasPasswordFor ( entry.site, entry.login ) model.syncData then
                model |> noCmd
            else
                -- add new password entry
                model |> updateNotifications (Notifications.newSiteEntry entry True)

        UpdateNotifications n ->
            { model | notificationsView = n } |> noCmd

        UpdatePasswordView m ->
            { model | passwordsView = Views.Passwords.update m model.passwordsView } |> noCmd

        FillForm config ->
            model |> withCmds [ Ports.fillForm config ]
    )
        |> (\( newModel, cmds ) -> ( newModel, Cmd.batch [ cmds, Ports.sendOutNewState (Model.encode newModel) ] ))


saveEntry : String -> SiteEntry -> Model -> ( Model, Cmd Msg )
saveEntry groupId entry model =
    { model
        | newSiteEntry = Data.PasswordMeta.reset model.newSiteEntry
        , expandSiteEntry = False
    }
        |> withCmds [ withTimestamp (InsertSite ( entry.site, entry.login ) ( entry.securityLevel, groupId ) entry.password) ]


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
