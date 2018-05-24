module Background exposing (..)

import Random.Pcg.Extended as RandomE
import Navigation exposing (Location)
import Toasty
import Time
import Timer
import Set


--

import Helper exposing (withCmds, noCmd, andThenUpdate, andThenUpdateIf, addCmds, mapModel, andThenCmd)
import Ports
import Route exposing (Page(..))
import Data.PasswordMeta
import Data.Notifications as Notifications exposing (SiteEntry)
import Data.Sync
import Data.Storage
import Data.Settings
import Data.KeyBox
import Protocol.Api as Api
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Passwords
import Views.CreateKeyBox
import Views.Devices
import Views.Settings
import MainView
import Model exposing (..)


-- Init


init : Flags -> Location -> ( ModelState, Cmd Msg )
init flags location =
    let
        newState =
            Model.init flags location
    in
        case newState of
            Loaded model ->
                let
                    -- Kind of a hack to support group 1:
                    -- Just unlock group 1 at the very beginning (if it exists)
                    -- If the group is locked there are some weired edge cases that are avoided by unlocking it here.
                    -- We also have to remember to unlock it as soon as it becomes available, e.g. after we receive the key.
                    newSync =
                        Data.Sync.unlockGroup1IfExists model.syncData

                    newModel =
                        { model | syncData = newSync }
                in
                    Loaded newModel |> withCmds [ onLoaded newModel, setTitle newModel.syncData ]

            _ ->
                newState |> noCmd


onLoaded : Model -> Cmd Msg
onLoaded model =
    Cmd.batch [ storeState model, Api.askForNewVersion model.syncData ]



-- Routing


locationToMsg : Location -> Msg
locationToMsg location =
    Route.fromLocation location |> SetPage



-- Update


updateModelState : Msg -> ModelState -> ( ModelState, Cmd Msg )
updateModelState msg state =
    (case state of
        LoadingError err ->
            -- TODO: diplay error, and help solve it, at least offer backup
            state |> noCmd

        Loaded model ->
            update msg model
                |> (\( m, cmd ) -> ( Loaded m, Cmd.batch [ cmd, setTitle m.syncData ] ))
    )
        |> (\( newModel, cmds ) ->
                ( newModel
                , Cmd.batch [ cmds, Ports.sendOutNewState (Model.encode newModel) ]
                )
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            { model | currentPage = page } |> noCmd

        DoneWithTutorial ->
            let
                ret =
                    updateSettings Data.Settings.setDoneWithTutorial model
            in
                case model.currentPage of
                    Tutorial ->
                        ret |> addCmds [ Navigation.back 1 ]

                    _ ->
                        ret

        OnGotOnlineStatus isOnline ->
            -- The browser tells us we are online, lets ask other devices for their version
            { model | connectedToServer = isOnline && model.connectedToServer }
                |> withCmds [ Api.askForNewVersion model.syncData ]

        NavigateTo page ->
            navigateTo page model

        NavigateBack ->
            model |> withCmds [ Navigation.back 1 ]

        OnStateRequest ->
            -- Since we send our state out on every update, we don't need to do anything here
            model |> noCmd

        UpdatePassword ( level, groupId ) ( siteName, userName ) pw ->
            { model | passwordsView = Views.Passwords.finishEdit model.passwordsView }
                |> saveEntry groupId { login = userName, site = siteName, password = pw, securityLevel = level }
                |> mapModel updateSeed

        MovePassword accountId from to ->
            model |> withCmds [ Helper.withTimestamp (DoMovePassword accountId from to) ]

        DoMovePassword accountId from to time ->
            let
                ( newSync, groupsToRequest ) =
                    Data.Sync.movePassword time accountId from to model.syncData
            in
                { model | syncData = newSync }
                    |> Api.syncToOthers
                    |> andThenUpdate (Api.requestShares groupsToRequest)

        AddPassword pw ->
            let
                group =
                    Data.Sync.currentGroupId model.newSiteEntry.securityLevel model.syncData

                { userName, siteName, securityLevel } =
                    model.newSiteEntry
            in
                saveEntry group { login = userName, site = siteName, password = pw, securityLevel = securityLevel } model
                    |> mapModel updateSeed
                    |> addCmds [ Navigation.back 1 ]

        SaveEntry id groupId entry ->
            saveEntry groupId entry model
                |> andThenUpdate (updateNotifications (Notifications.remove id))
                |> andThenCmd closePopup

        InsertSite accountId groupId password timestamp ->
            let
                ( newSync, newSeed, shouldRequestShare, cmd ) =
                    Data.Sync.insertSite encryptNewShares timestamp model.seed accountId groupId password model.syncData
            in
                { model | syncData = newSync, seed = newSeed }
                    |> Api.syncToOthers
                    |> addCmds [ cmd ]
                    |> andThenUpdateIf shouldRequestShare (Api.requestShares [ groupId ])

        ImportPasswords pws groupId timestamp ->
            let
                ( nSync, nSeed, nShouldRequestShare, nCmds ) =
                    List.foldl
                        (\{ site, login, password } ( sync, seed, shouldRequestShare, cmds ) ->
                            let
                                ( newSync, newSeed, newShouldRequestShare, cmd ) =
                                    Data.Sync.insertSite encryptNewShares timestamp seed ( site, login ) groupId password sync
                            in
                                ( newSync, newSeed, newShouldRequestShare || shouldRequestShare, cmd :: cmds )
                        )
                        ( model.syncData, model.seed, False, [] )
                        pws
            in
                { model | syncData = nSync, seed = nSeed }
                    |> Api.syncToOthers
                    |> addCmds nCmds
                    |> andThenUpdateIf nShouldRequestShare (Api.requestShares [ groupId ])

        DismissNotification id ->
            model
                |> updateNotifications (Notifications.remove id)
                |> andThenCmd closePopup

        DeactivateForSite id site ->
            { model | syncData = Data.Sync.deactivateForSite site model.syncData }
                |> updateNotifications (Notifications.remove id)
                |> andThenUpdate Api.syncToOthers

        RemoveFromIgnored site ->
            { model | syncData = Data.Sync.removeFromIgnored site model.syncData }
                |> Api.syncToOthers

        SiteNameChanged s ->
            { model
                | newSiteEntry = (\e -> { e | siteName = s }) model.newSiteEntry
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

        ProtocolMsg pMsg ->
            Api.update model pMsg

        GetTokenClicked ->
            getToken model

        UpdatePairing s ->
            { model | pairingDialogue = s }
                |> noCmd

        DoTokenSubmitted time ->
            model
                |> updatePairingDialogue Views.Pairing.tokenSubmitted
                |> Api.pairWith model.uniqueIdentifyier time

        TokenSubmitted ->
            model
                |> withCmds [ Helper.withTimestamp DoTokenSubmitted ]

        RemoveDevice uuid ->
            model |> withCmds [ Helper.withTimestamp (DoRemoveDevice uuid) ]

        DoRemoveDevice uuid time ->
            let
                ( sync, removeCmd ) =
                    Api.removeDevice time uuid model.syncData
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

        RequestPasswordPressed keys mayFill ->
            let
                ( newSync, mayForm ) =
                    Data.Sync.requestPasswordPressed keys mayFill model.syncData

                newModel =
                    { model | syncData = newSync }
            in
                case mayForm of
                    Just formData ->
                        -- The password is already here
                        newModel
                            |> withCmds [ Ports.fillForm formData ]

                    Nothing ->
                        newModel
                            |> Api.requestShares keys
                            -- TODO: add time to options
                            -- TODO: this can behave unexpected:
                            -- Lock group 2, wait 9 min, unlock group 2 and it will close in 1 min!
                            |> addCmds [ Timer.setTimeOut (LockGroups keys) (10 * Time.minute) ]

        LockGroups groups ->
            { model | syncData = Data.Sync.lockGroups groups model.syncData } |> noCmd

        GrantShareRequest id req ->
            model
                |> updateNotifications (Notifications.remove id)
                |> addCmds [ Api.grantRequest req model.syncData ]
                |> andThenCmd closePopup

        RejectShareRequest id req ->
            -- inform other such that they stop asking
            model
                |> updateNotifications (Notifications.remove id)
                |> addCmds [ Api.rejectShareRequest req.deviceId req.reqIds ]
                |> andThenCmd closePopup

        ResetDevice ->
            Model.reset model
                |> withCmds [ resetStorage (Model.reset model) ]

        UpdateSettingsView state ->
            { model | settingsView = state }
                |> noCmd

        SendOutAccountsFor site ->
            let
                newSiteEntry =
                    if model.newSiteEntry.siteName == "" || Just model.newSiteEntry.siteName == model.currentSite then
                        Data.PasswordMeta.setSiteName site model.newSiteEntry
                    else
                        model.newSiteEntry
            in
                { model | currentSite = Just site, newSiteEntry = newSiteEntry }
                    |> withCmds [ Ports.accountsForSite (Data.Sync.getAccountsForSite site model.syncData) ]

        AddSiteEntry { isSignUp, entry } ->
            -- TODO: type can be either SignUp | LogIn | UpdateCredentials
            -- LogIn can be ignored if we already have an entry for it,
            -- TODO: unless the group is unlocked and the entered password differs from the one we saved.
            if Data.Sync.numberOfAvailableDevices model.syncData < Data.Sync.minSecurityLevel model.syncData then
                model |> noCmd
            else if not isSignUp && Data.Sync.hasPasswordFor ( entry.site, entry.login ) model.syncData then
                model |> noCmd
            else
                (-- add new password entry, but only if we don't ignore the current site
                 if Set.member entry.site (Data.Sync.getSettings model.syncData).deactivateForSite then
                    model |> noCmd
                 else
                    model |> updateNotifications (Notifications.newSiteEntry entry True)
                )

        SetSettings settings ->
            setSettings settings model

        DoSetSettings options time ->
            { model | syncData = Data.Sync.setSettings time options model.syncData }
                |> Api.syncToOthers

        UpdateNotifications n ->
            { model | notificationsView = n } |> noCmd

        UpdatePasswordView m ->
            { model | passwordsView = Views.Passwords.update m model.passwordsView } |> noCmd

        UpdateDevicesView m ->
            { model | devicesView = m } |> noCmd

        FillForm (( siteName, userName ) as accountId) ->
            case Data.Sync.getPassword accountId model.syncData of
                Just pw ->
                    model |> withCmds [ Ports.fillForm { site = siteName, login = userName, password = pw } ]

                Nothing ->
                    model |> noCmd

        ReceiveMyShares myShares ->
            { model | syncData = Data.Sync.addToMyShares myShares model.syncData } |> noCmd

        NewEncryptedShares { time, groupId, shares } ->
            { model | syncData = Data.Sync.addNewShares time groupId shares model.syncData }
                |> Api.syncToOthers

        SharesReadyToSend r ->
            model
                |> withCmds [ Api.sendGrantedRequest r ]

        DidDecryptRequestedShares r ->
            model |> Api.grantedShareRequest r

        ToastyMsg subMsg ->
            Toasty.update MainView.toastyConfig ToastyMsg subMsg model

        ShowToast txt ->
            ( model, Cmd.none )
                |> Toasty.addToast MainView.toastyConfig ToastyMsg txt

        ScanQR ->
            model |> withCmds [ Ports.scanQR () ]

        -- AddDrivePressed ->
        --     -- TODO:
        --     model |> withCmds [ Navigation.load "https://www.googleapis.com/auth/drive.appfolder" ]
        UpdateCreateKeyBox state ->
            { model | createKeyBoxView = state } |> noCmd

        StartCreatingKeyBox params ->
            model |> withCmds [ Ports.hashPwFirst params ]

        DoCreateKeyBox { name, key, salt, passwordHash, hashSalt, time } ->
            let
                newSync =
                    Data.Sync.updateKeyBoxes
                        (Data.KeyBox.createBox
                            { creatorId = model.syncData.id
                            , name = name
                            , key = key
                            , salt = salt
                            , passwordHash = passwordHash
                            , hashSalt = hashSalt
                            }
                            time
                        )
                        model.syncData

                -- Create shares for the new box for all currently unlocked groups
                ( newSync2, encryptCmd ) =
                    Data.Sync.createNewSharesIfPossible encryptNewShares time newSync
            in
                { model | syncData = newSync2 }
                    |> withCmds [ Navigation.back 1, encryptCmd ]
                    |> andThenUpdate Api.syncToOthers

        OpenBox box pw ->
            model |> withCmds [ Ports.openBox { boxId = box.id, password = pw, salt = box.salt, hashSalt = box.hashSalt } ]

        DoOpenBox { boxId, key, passwordHash, time } ->
            let
                mayBox =
                    Data.KeyBox.openBox boxId { key = key, passwordHash = passwordHash } (Data.Sync.getKeyBoxes model.syncData)
            in
                case mayBox of
                    Ok b ->
                        let
                            newSync =
                                Data.Sync.updateKeyBoxes (always b) model.syncData

                            ( newSync2, encryptCmd ) =
                                Data.Sync.createNewSharesIfPossible encryptNewShares time newSync
                        in
                            { model | syncData = newSync2 }
                                |> withCmds [ encryptCmd ]
                                |> andThenUpdate Api.syncToOthers

                    Err e ->
                        { model | devicesView = Views.Devices.wrongPassword e model.devicesView }
                            |> noCmd

        CloseBox id ->
            { model
                | syncData = Data.Sync.updateKeyBoxes (Data.KeyBox.closeBox id) model.syncData
                , devicesView = Views.Devices.closeBox model.devicesView
            }
                |> noCmd

        ExportPasswords ->
            let
                sync =
                    Data.Sync.exportPasswords model.syncData
            in
                { model | syncData = sync } |> navigateTo Passwords

        CancelExportPassword ->
            { model | syncData = Data.Sync.cancelExportPassword model.syncData } |> noCmd

        OnImportPasswords ({ contents, filename } as data) ->
            let
                mPws =
                    Data.Sync.parsePasswords contents

                groupId =
                    Data.Sync.currentGroupId 2 model.syncData
            in
                case mPws of
                    Ok pws ->
                        model
                            |> withCmds [ Helper.withTimestamp (ImportPasswords pws ( 2, groupId )) ]
                            |> andThenUpdate (navigateTo Passwords)

                    Err e ->
                        { model | settingsView = Views.Settings.parseFileError e model.settingsView } |> noCmd

        OnGotQR code ->
            model
                |> updatePairingDialogue (Views.Pairing.setInputToken code)
                |> withCmds [ Helper.withTimestamp DoTokenSubmitted ]

        OpenExtensionInTab ->
            model |> withCmds [ Ports.openExtensionInTab () ]


encryptNewShares time group shares =
    if List.isEmpty shares then
        Cmd.none
    else
        Ports.encryptNewShares { time = time, groupId = group, shares = shares }


setSettings : Data.Settings.Settings -> Model -> ( Model, Cmd Msg )
setSettings settings model =
    model
        |> withCmds [ Helper.withTimestamp (DoSetSettings settings) ]


updateSettings : (Data.Settings.Settings -> Data.Settings.Settings) -> Model -> ( Model, Cmd Msg )
updateSettings f model =
    setSettings (f (Data.Sync.getSettings model.syncData)) model


navigateTo : Page -> Model -> ( Model, Cmd Msg )
navigateTo page model =
    let
        doNothing model =
            ( model, Cmd.none )

        ( navFn, updateFn ) =
            case ( model.currentPage, page ) of
                ( _, Pairing ) ->
                    ( Route.newUrl, getToken )

                ( _, ReleaseLog s ) ->
                    ( Route.newUrl, updateSettings (Data.Settings.markSeen s) )

                ( Home, _ ) ->
                    ( Route.newUrl, doNothing )

                ( from, destination ) ->
                    if Route.hasBackButton destination then
                        ( Route.newUrl, doNothing )
                    else if Route.hasBackButton from then
                        ( Route.modifyUrl, doNothing )
                    else
                        ( Route.modifyUrl, doNothing )
    in
        { model
            | settingsView = Views.Settings.clear model.settingsView
            , passwordsView = Views.Passwords.clear model.passwordsView
            , devicesView = Views.Devices.clear model.devicesView
            , createKeyBoxView = Views.CreateKeyBox.clear model.createKeyBoxView
        }
            |> withCmds [ navFn page ]
            |> andThenUpdate updateFn


getToken : Model -> ( Model, Cmd Msg )
getToken model =
    model
        |> updatePairingDialogue Views.Pairing.getTockenClicked
        |> Api.initPairing model.uniqueIdentifyier


saveEntry : String -> SiteEntry -> Model -> ( Model, Cmd Msg )
saveEntry groupId entry model =
    let
        n =
            Data.Sync.numberOfAvailableDevices model.syncData

        minSecLevel =
            Data.Sync.minSecurityLevel model.syncData
    in
        { model | newSiteEntry = Data.PasswordMeta.reset model.newSiteEntry }
            |> withCmds [ Helper.withTimestamp (InsertSite ( entry.site, entry.login ) ( clamp minSecLevel (min 5 n) entry.securityLevel, groupId ) entry.password) ]


updateSeed : Model -> Model
updateSeed model =
    -- We need this to advance the seed so that after we created and accepted a password,
    -- we get a different series of random passswords
    { model
        | seed = RandomE.step (RandomE.int 1 42) model.seed |> Tuple.second
        , requirementsState = PW.nextPassword model.requirementsState
    }


resetStorage : Model -> Cmd Msg
resetStorage model =
    Ports.resetStorage (Data.Storage.encode model)


storeState : Model -> Cmd Msg
storeState model =
    Ports.storeState (Data.Storage.encode model)


setTitle : Data.Sync.SyncData -> Cmd Msg
setTitle sync =
    let
        ( name, post ) =
            Data.Sync.getName sync
    in
        Ports.setTitle
            (if name == "" && post == "" then
                "NoKey"
             else if name == "" && post /= "" then
                "NoKey (" ++ post ++ ")"
             else if name /= "" && post == "" then
                "NoKey (" ++ name ++ ")"
             else
                "NoKey (" ++ name ++ " (" ++ post ++ "))"
            )



-- Subs


subs : ModelState -> Sub Msg
subs state =
    ((case state of
        Loaded model ->
            [ Api.connectPrivateSocket model.uniqueIdentifyier
            , Api.onSignedMsg
            , Api.onAuthenticatedMsg
            , Ports.onReceiveMyShares ReceiveMyShares
            , Ports.onNewEncryptedShares NewEncryptedShares
            , Ports.onDidEncryptShares SharesReadyToSend
            , Ports.onDidDecryptRequestedShares DidDecryptRequestedShares
            , Ports.onGotQR OnGotQR
            , Ports.onGotOnlineStatus OnGotOnlineStatus
            , Ports.onFileContentRead OnImportPasswords
            , Ports.didHashPwFirst DoCreateKeyBox
            , Ports.onDidOpenBox DoOpenBox
            ]

        _ ->
            []
     )
        ++ [ Ports.onStateRequest (always OnStateRequest)
           , Ports.onReceiveMsg Model.decodeMsg
           , Ports.onRequestAccountsForSite SendOutAccountsFor
           , Ports.onAddSiteEntry AddSiteEntry
           ]
    )
        |> Sub.batch
