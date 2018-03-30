module MainView exposing (view)

import Html exposing (Html)
import Element exposing (..)
import Element.Lazy exposing (lazy)
import Dict


--

import Styles
import Elements
import Data.Sync
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
import Views.Passwords
import Model exposing (Msg(..), ModelState(..))


view : ModelState -> Html Msg
view state =
    (case state of
        Loaded model ->
            let
                numberOfKnownDevices =
                    Data.Sync.knownIds model.syncData |> Dict.size
            in
                column []
                    -- the nesting seems to be nececairy to get the correct centering + max width behaviour
                    [ row
                        [ centerX, width (fillBetween { min = Just 320, max = Just 1024 }) ]
                        -- TODO: display notification on top of other as popup, (card with high elevation)
                        [ column [ width fill ]
                            (if Notifications.count model.notifications > 0 then
                                [ Views.Notifications.view notificationsConfig
                                    model.syncData
                                    model.notifications
                                    numberOfKnownDevices
                                    model.notificationsView
                                ]
                             else
                                [ Views.Devices.view model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
                                , Elements.line
                                , -- TODO: consider if we should just automatically request a pairing code
                                  -- upon navigating to this view. This way a user doesn't have to decide what to press
                                  Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
                                , Elements.line
                                , if numberOfKnownDevices >= 2 then
                                    newSiteForm model.requirementsState
                                        model.expandSiteEntry
                                        model.newSiteEntry
                                        (Data.Sync.currentGroupId model.newSiteEntry.securityLevel model.syncData)
                                        numberOfKnownDevices
                                  else
                                    text "pair a device to save your first password"
                                , Elements.line
                                , lazy (Views.Passwords.view passwordsConfig model.passwordsView) model.syncData
                                , Elements.line
                                , Elements.button (Just ResetDevice) "Reset Device"
                                ]
                            )
                        ]
                    ]

        LoadingError err ->
            column []
                [ Elements.h2 "Error"
                , Elements.p err
                ]
    )
        |> Element.layout Styles.background


passwordsConfig : Views.Passwords.Config Msg
passwordsConfig =
    { toMsg = UpdatePasswordView
    , onDeletePassword = DeletePassword
    , onRequestPasswordPressed = RequestPasswordPressed
    , onTogglePassword = TogglePassword
    }


notificationsConfig : Views.Notifications.Config Msg
notificationsConfig =
    { onRejectRequest = RejectShareRequest
    , onGrantRequest = GrantShareRequest
    , onDismiss = DismissNotification
    , onSaveEntry = SaveEntry
    , toMsg = UpdateNotifications
    }


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> String -> Int -> Element Msg
newSiteForm requirementsState expandSiteEntry entry currentGroupId maxSecurityLevel =
    column []
        [ column []
            [ Elements.inputWithLabel (Just SiteNameChanged) "New Site: " "example.com" entry.siteName
            ]
        , if not expandSiteEntry then
            empty
          else
            column []
                [ Elements.inputWithLabel (Just UserNameChanged) "Login name" "" entry.userName
                , Elements.text "Security Level: "
                , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                , Views.PasswordGenerator.view (AddPassword currentGroupId) NewPasswordRequirements requirementsState
                ]
        ]
