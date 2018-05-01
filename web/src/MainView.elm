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
import Views.Options
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
import Views.Tutorial
import Views.Passwords
import Views.NewPassword
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
                , width (fillBetween { min = Just 320, max = Just 1024 })
                , height fill
                , padding (Styles.paddingScale 3)
                , scrollbars
                ]
    in
        -- TODO: hack:
        -- I set the height here to get scroll behaviour to work, see
        -- https://github.com/mdgriffith/stylish-elephants/issues/30
        column [ unselectable ]
            -- the nesting seems to be nececairy to get the correct centering + max width behaviour
            [ row
                [ heightHack, height fill ]
                -- TODO: display notification on top of other as popup, (card with high elevation)
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
                        [ viewTitle model.currentPage model.toasties
                        , cont
                            [ viewPage model.currentPage model ]
                        , bottomNavigation model.currentPage model
                        ]
                    )
                ]
            ]


unselectable : Attribute msg
unselectable =
    htmlAttribute
        (HtmlAttr.style
            [ ( "-webkit-touch-callout", "none" )
            , ( "-webkit-user-select", "none" )
            , ( "-khtml-user-select", "none" )
            , ( "-moz-user-select", "none" )
            , ( "-ms-user-select", "none" )
            , ( "user-select", "none" )
            ]
        )


viewTitle : Page -> Model.Toast -> Element Msg
viewTitle page toasties =
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

        attr =
            [ below (html toast), width (fillBetween { min = Just 320, max = Just 1024 }), centerX ]
    in
        (if Route.hasBackButton page then
            row attr [ el [ alignLeft, width fill ] (Elements.backButton NavigateBack), title, el [ width fill ] empty ]
         else
            el attr title
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
             , above (el [ alignRight, paddingXY (Styles.paddingScale 3) (Styles.paddingScale 5) ] (viewActionButton page model))
             ]
            )
        |> (\c ->
                row (width fill :: Styles.cardShadow 2)
                    [ column [ width fill ]
                        [ column
                            [ width (fillBetween { min = Just 320, max = Just 1024 })
                            , centerX
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
            Views.Devices.actionButton

        _ ->
            empty


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
            Views.Devices.view model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)

        Options ->
            Views.Options.view model

        Pairing ->
            -- , -- TODO: consider if we should just automatically request a pairing code
            --   -- upon navigating to this view. This way a user doesn't have to decide what to press
            Views.Pairing.view pairingConfig (Data.Sync.isAndroid model.syncData) model.pairingDialogue

        Tutorial ->
            Views.Tutorial.view

        NewPassword ->
            Views.NewPassword.view model


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
    }


notificationsConfig : Views.Notifications.Config Msg
notificationsConfig =
    { onRejectRequest = RejectShareRequest
    , onGrantRequest = GrantShareRequest
    , onDismiss = DismissNotification
    , onSaveEntry = SaveEntry
    , toMsg = UpdateNotifications
    }
