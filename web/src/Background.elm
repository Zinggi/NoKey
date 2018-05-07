module Background exposing (..)

import Random.Pcg.Extended as RandomE
import Navigation exposing (Location)
import Toasty
import Time
import Timer


--

import Helper exposing (withCmds, noCmd, andThenUpdate, andThenUpdateIf, addCmds, mapModel, andThenCmd)
import Ports
import Route exposing (Page(..))
import Data.PasswordMeta
import Data.Notifications as Notifications exposing (SiteEntry)
import Data.Sync
import Data.Storage
import Data.Settings
import Protocol.Api as Api
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Passwords
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
                newModel =
                    { model | isFirstTimeUser = False }

                ret =
                    newModel |> withCmds [ storeState newModel ]
            in
                case model.currentPage of
                    Tutorial ->
                        ret
                            |> addCmds [ Navigation.back 1 ]

                    _ ->
                        ret

        OnGotOnline ->
            -- The browser tells us we are online, lets ask other devices for their version
            model |> withCmds [ Api.askForNewVersion model.syncData ]

        NavigateTo page ->
            let
                doNothing model =
                    ( model, Cmd.none )

                ( navFn, updateFn ) =
                    case ( model.currentPage, page ) of
                        ( _, Pairing ) ->
                            ( Route.newUrl, getToken )

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
                model
                    |> withCmds [ navFn page ]
                    |> andThenUpdate updateFn

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
                    Data.Sync.insertSite encryptShares timestamp model.seed accountId groupId password model.syncData

                -- Here we should encrypt the shares for the others
                encryptShares time groupId_ shares =
                    Ports.encryptNewShares { time = time, groupId = groupId_, shares = shares }
            in
                { model | syncData = newSync, seed = newSeed }
                    |> Api.syncToOthers
                    |> addCmds [ cmd ]
                    |> andThenUpdateIf shouldRequestShare (Api.requestShares [ groupId ])

        DismissNotification id ->
            model
                |> updateNotifications (Notifications.remove id)
                |> andThenCmd closePopup

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
                        (\e -> { e | siteName = site }) model.newSiteEntry
                    else
                        model.newSiteEntry
            in
                { model | currentSite = Just site, newSiteEntry = newSiteEntry }
                    |> withCmds [ Ports.accountsForSite (Data.Sync.getAccountsForSite site model.syncData) ]

        AddSiteEntry { isSignUp, entry } ->
            -- TODO: type can be either SignUp | LogIn | UpdateCredentials
            -- LogIn can be ignored if we already have an entry for it,
            -- TODO: unless the group is unlocked and the entered password differs from the one we saved.
            if Data.Sync.numberOfKnownDevices model.syncData < Data.Sync.minSecurityLevel model.syncData then
                model |> noCmd
            else if not isSignUp && Data.Sync.hasPasswordFor ( entry.site, entry.login ) model.syncData then
                model |> noCmd
            else
                -- add new password entry
                model |> updateNotifications (Notifications.newSiteEntry entry True)

        SetSettings options ->
            model
                |> withCmds [ Helper.withTimestamp (DoSetSettings options) ]

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

        OnGotQR code ->
            model
                |> updatePairingDialogue (Views.Pairing.setInputToken code)
                |> withCmds [ Helper.withTimestamp DoTokenSubmitted ]


getToken : Model -> ( Model, Cmd Msg )
getToken model =
    model
        |> updatePairingDialogue Views.Pairing.getTockenClicked
        |> Api.initPairing model.uniqueIdentifyier


closePopup : Model -> Cmd Msg
closePopup model =
    if Notifications.count model.notifications <= 0 then
        Ports.closePopup ()
    else
        Cmd.none


saveEntry : String -> SiteEntry -> Model -> ( Model, Cmd Msg )
saveEntry groupId entry model =
    let
        n =
            Data.Sync.numberOfKnownDevices model.syncData

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
            , Ports.onGotOnline (always OnGotOnline)
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
