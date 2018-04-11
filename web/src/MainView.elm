module MainView exposing (view)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Element exposing (..)
import Element.Lazy exposing (lazy)
import Dict


--

import Styles
import Elements
import Data.Sync
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
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

                    {- Just { color = Styles.accentColor, offset = ( 0, 0 ), blur = 2, size = 0 } -}
                    }
                ]
            }
            Styles.background


viewModel : Model -> Element Msg
viewModel model =
    let
        numberOfKnownDevices =
            Data.Sync.knownIds model.syncData |> Dict.size
    in
        column [ unselectable ]
            -- the nesting seems to be nececairy to get the correct centering + max width behaviour
            [ row
                [ centerX, width (fillBetween { min = Just 320, max = Just 1024 }), height fill ]
                -- TODO: display notification on top of other as popup, (card with high elevation)
                [ column [ width fill, height fill ]
                    (if Notifications.count model.notifications > 0 then
                        [ Views.Notifications.view notificationsConfig
                            model.syncData
                            model.notifications
                            numberOfKnownDevices
                            model.notificationsView
                        ]
                     else
                        [ viewTitle model.currentPage
                        , column [ height fill, padding (Styles.paddingScale 3), scrollbars ]
                            -- TODO: I cant get this to work, maybe it's this:
                            -- https://github.com/mdgriffith/stylish-elephants/issues/30
                            [ viewPage model.currentPage model ]
                        , buttomNavigation model.currentPage
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


viewTitle : Page -> Element Msg
viewTitle page =
    let
        title =
            el [ padding (Styles.paddingScale 3) ] (Elements.h2 (Route.pageToTitle page))
    in
        if Route.hasBackButton page then
            row [] [ el [ alignLeft, width fill ] (Elements.backButton NavigateBack), title, el [ width fill ] empty ]
        else
            title


buttomNavigation : Page -> Element Msg
buttomNavigation page =
    [ Home, Devices, Passwords, Options ]
        |> List.map (\p -> Elements.pageButton (NavigateTo p) (page == p) p)
        |> row [ padding (Styles.paddingScale 1), alignBottom ]


viewPage : Page -> Model -> Element Msg
viewPage page model =
    case page of
        Home ->
            Views.Dashboard.view model

        Passwords ->
            case Views.Dashboard.needsPairingHint model of
                Just hint ->
                    hint

                Nothing ->
                    Views.Passwords.view passwordsConfig model

        Devices ->
            Views.Devices.view model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)

        Options ->
            Views.Options.view model

        Pairing ->
            -- , -- TODO: consider if we should just automatically request a pairing code
            --   -- upon navigating to this view. This way a user doesn't have to decide what to press
            Views.Pairing.view pairingConfig model.pairingDialogue

        Tutorial ->
            Views.Tutorial.view

        NewPassword ->
            Views.NewPassword.view model


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


passwordsConfig : Views.Passwords.Config Msg
passwordsConfig =
    { toMsg = UpdatePasswordView
    , onDeletePassword = DeletePassword
    , onRequestPasswordPressed = RequestPasswordPressed
    , onTogglePassword = TogglePassword
    , onAddNewPassword = NavigateTo NewPassword
    }


notificationsConfig : Views.Notifications.Config Msg
notificationsConfig =
    { onRejectRequest = RejectShareRequest
    , onGrantRequest = GrantShareRequest
    , onDismiss = DismissNotification
    , onSaveEntry = SaveEntry
    , toMsg = UpdateNotifications
    }
