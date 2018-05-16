module MainView exposing (view, toastyConfig)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Element exposing (..)
import Element.Lazy exposing (lazy)
import Dict
import Toasty


--

import Styles
import Elements
import Data.Sync
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
import Data.Settings
import Views.Dashboard
import Views.Settings
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
import Views.Tutorial
import Views.Passwords
import Views.NewPassword
import Views.ReleaseLog
import Views.CreateKeyBox
import Model exposing (Msg(..), ModelState(..), Model)
import Route exposing (Page(..))


view : ModelState -> Html Msg
view state =
    (case state of
        Loaded model ->
            viewModel model

        LoadingError err ->
            column [ padding (Styles.paddingScale 3) ]
                [ Elements.h2 "Error"
                , Elements.p err
                ]
    )
        |> Element.layoutWith
            { options =
                [ focusStyle
                    { borderColor = Just Styles.accentColor
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            Styles.background


heightHack =
    htmlAttribute (HtmlAttr.style [ ( "max-height", "100vh" ) ])


hackMinMaxWidthCentred : Int -> Int -> Attribute msg
hackMinMaxWidthCentred min max =
    htmlAttribute <|
        HtmlAttr.style
            [ ( "max-width", toString max ++ "px" )
            , ( "min-width", toString min ++ "px" )
            , ( "margin", "auto" )
            , ( "width", "100%" )
            ]


viewModel : Model -> Element Msg
viewModel model =
    let
        numberOfKnownDevices =
            Data.Sync.numberOfKnownDevices model.syncData

        minSecLevel =
            Data.Sync.minSecurityLevel model.syncData

        cont =
            column
                [ centerX
                , width (fill |> minimum 320 |> maximum 1024)
                , hackMinMaxWidthCentred 320 1024
                , height fill
                , padding (Styles.paddingScale 3)
                , scrollbars
                ]
    in
        -- TODO: hack:
        -- I set the height here to get scroll behaviour to work, see
        -- https://github.com/mdgriffith/stylish-elephants/issues/30
        column [ Styles.unselectable ]
            -- the nesting seems to be nececairy to get the correct centering + max width behaviour
            [ row
                [ heightHack, height fill ]
                [ column [ width fill, height fill, scrollbars ]
                    (if Notifications.count model.notifications > 0 then
                        [ cont
                            [ Views.Notifications.view notificationsConfig
                                model.syncData
                                model.notifications
                                ( minSecLevel, numberOfKnownDevices )
                                model.notificationsView
                            ]
                        ]
                     else
                        [ viewTitle model.connectedToServer model.currentPage model.toasties
                        , cont
                            [ viewPage model.currentPage model ]
                        , bottomNavigation model.currentPage model
                        ]
                    )
                ]
            ]


viewTitle : Bool -> Page -> Model.Toast -> Element Msg
viewTitle connectedToServer page toasties =
    let
        title =
            el [ padding (Styles.paddingScale 3) ] (Elements.h2 (Route.pageToTitle page))

        toast =
            Toasty.view toastyConfig renderToast ToastyMsg toasties

        renderToast txt =
            Html.div
                [ HtmlAttr.style
                    [ ( "padding", "1em" )
                    , ( "border-radius", "4px" )
                    , ( "cursor", "pointer" )
                    , ( "box-shadow", "0 5px 5px -5px rgba(0, 0, 0, 0.5)" )
                    , ( "color", "white" )
                    , ( "background", "#1998C2" )
                    , ( "font-size", "13px" )
                    ]
                ]
                [ Html.text txt ]

        onlineStatus =
            el [ alignRight ] (Elements.onlineDot connectedToServer)

        attr =
            [ below (html toast), width (fill |> minimum 320 |> maximum 1024), centerX, hackMinMaxWidthCentred 320 1024 ]
    in
        (if Route.hasBackButton page then
            row attr [ el [ alignLeft, width fill ] (Elements.backButton NavigateBack), title, el [ width fill ] onlineStatus ]
         else
            row attr [ title, onlineStatus ]
        )
            |> (\c -> row (width fill :: Styles.cardShadow 2 ++ Styles.titleHighlight) [ column [ width fill ] [ c ] ])


toastyConfig : Toasty.Config msg
toastyConfig =
    Toasty.config
        |> Toasty.transitionOutDuration 1000
        |> Toasty.delay 5000
        |> Toasty.containerAttrs toastContainerAttrs
        |> Toasty.itemAttrs toastItemAttrs
        |> Toasty.transitionInAttrs transitionInAttrs
        |> Toasty.transitionOutAttrs transitionOutAttrs


toastContainerAttrs : List (Html.Attribute msg)
toastContainerAttrs =
    [ HtmlAttr.style
        [ ( "position", "fixed" )
        , ( "top", "50px" )
        , ( "right", "0" )
        , ( "width", "200px" )
        , ( "list-style-type", "none" )
        , ( "padding", "0" )
        , ( "margin", "0" )
        ]
    ]


toastItemAttrs : List (Html.Attribute msg)
toastItemAttrs =
    [ HtmlAttr.style
        [ ( "margin", "1em 1em 0 1em" )
        , ( "max-height", "100px" )
        , ( "transition", "max-height 0.6s, margin-top 0.6s" )
        ]
    ]


transitionInAttrs : List (Html.Attribute msg)
transitionInAttrs =
    [ HtmlAttr.class "animated fast fadeInRight"
    ]


transitionOutAttrs : List (Html.Attribute msg)
transitionOutAttrs =
    [ HtmlAttr.class "animated fadeOutRightBig"
    , HtmlAttr.style
        [ ( "max-height", "0" )
        , ( "margin-top", "0" )
        ]
    ]


bottomNavigation : Page -> Model -> Element Msg
bottomNavigation page model =
    [ Home, Devices, Passwords, Options ]
        |> List.map (\p -> Elements.pageButton (NavigateTo p) (page == p) p)
        |> row
            ([ padding (Styles.paddingScale 1)
             , alignBottom
             , above (el [ alignRight, paddingXY (Styles.paddingScale 4) (Styles.paddingScale 5) ] (viewActionButton page model))
             ]
            )
        |> (\c ->
                row (width fill :: Styles.cardShadow 2)
                    [ column [ width fill ]
                        [ column
                            [ width (fill |> minimum 320 |> maximum 1024)
                            , centerX
                            , hackMinMaxWidthCentred 320 1024
                            ]
                            [ c ]
                        ]
                    ]
           )


viewActionButton : Page -> Model -> Element Msg
viewActionButton page model =
    case page of
        Passwords ->
            Views.Passwords.actionButton passwordsConfig model

        Devices ->
            Views.Devices.actionButton devicesConfig model.devicesView

        _ ->
            none


viewPage : Page -> Model -> Element Msg
viewPage page model =
    case page of
        Home ->
            Views.Dashboard.view passwordsConfig model

        Passwords ->
            case Views.Dashboard.needsPairingHint model of
                ( True, hint ) ->
                    hint

                _ ->
                    Views.Passwords.view passwordsConfig model

        Devices ->
            Views.Devices.view devicesConfig model model.devicesView

        Options ->
            Views.Settings.view settingsConfig model model.settingsView

        Pairing ->
            Views.Pairing.view pairingConfig (Data.Sync.isAndroid model.syncData) model.pairingDialogue

        Tutorial ->
            Views.Tutorial.view

        NewPassword ->
            Views.NewPassword.view model

        -- CreateKeyBox ->
        --     Views.CreateKeyBox.view keyBoxConfig model model.createKeyBoxView
        ReleaseLog s ->
            Views.ReleaseLog.view s



-- keyBoxConfig : Views.CreateKeyBox.Config Msg
-- keyBoxConfig =
--     { toMsg = UpdateCreateKeyBox, onCreateKeyBox = DoCreateKeyBox, onCancel = NavigateBack }


devicesConfig : Views.Devices.Config Msg
devicesConfig =
    { toMsg = UpdateDevicesView
    , onSetDeviceName = SetDeviceName
    , onGoToPairing = NavigateTo Pairing
    , onRemoveDevice = RemoveDevice

    -- , onCreateKeyBox = NavigateTo CreateKeyBox
    }


settingsConfig : Views.Settings.Config Msg
settingsConfig =
    { toMsg = UpdateSettingsView
    , onShowTutorial = NavigateTo Tutorial
    , onSetSettings = SetSettings
    , onReset = ResetDevice
    , onExportPasswords = ExportPasswords
    , onOpenExtensionInTab = OpenExtensionInTab
    , onShowReleaseLog = NavigateTo (ReleaseLog "")
    }


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing, onScanQR = ScanQR }


passwordsConfig : Views.Passwords.Config Msg
passwordsConfig =
    { toMsg = UpdatePasswordView
    , onDeletePassword = DeletePassword
    , onRequestPasswordPressed = RequestPasswordPressed
    , onTogglePassword = TogglePassword
    , onLockGroupsPressed = LockGroups
    , onAddNewPassword = NavigateTo NewPassword
    , addPassword = UpdatePassword
    , onNewPasswordRequirements = NewPasswordRequirements
    , movePw = MovePassword
    , onCopyToClipboard = ShowToast "Copied to clipboard"
    , onCancelExportPassword = CancelExportPassword
    }


notificationsConfig : Views.Notifications.Config Msg
notificationsConfig =
    { onRejectRequest = RejectShareRequest
    , onGrantRequest = GrantShareRequest
    , onDismiss = DismissNotification
    , onSaveEntry = SaveEntry
    , toMsg = UpdateNotifications
    }
