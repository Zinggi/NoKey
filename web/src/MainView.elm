module MainView exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Element exposing (..)
import Element.Keyed
import Element.Lazy exposing (lazy)
import Random.Pcg.Extended as Random exposing (Seed)
import SecretSharing


--

import Styles
import Elements
import Data.Sync exposing (SyncData)
import Data exposing (AccountId, GroupId)
import Data.RequestGroupPassword as RequestPassword exposing (Status(..), PasswordStatus(..))
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
import Views.Passwords
import Model exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        numberOfKnownDevices =
            Data.Sync.knownIds model.syncData |> List.length
    in
        row
            [ alignTop ]
            -- TODO: use width (fillBetween { min = Just 150, max = Just 800 })
            -- once https://github.com/mdgriffith/stylish-elephants/issues/54 is resolved
            [ column [ centerX, htmlAttribute (Attr.style [ ( "maxWidth", "800px" ) ]) ]
                (if Notifications.count model.notifications > 0 then
                    [ Views.Notifications.view notificationsConfig model.syncData model.notifications numberOfKnownDevices model.notificationsView ]
                 else
                    [ Views.Devices.view model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
                    , Elements.line
                    , Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
                    , Elements.line
                    , if numberOfKnownDevices >= 2 then
                        newSiteForm model.requirementsState
                            model.expandSiteEntry
                            model.newSiteEntry
                            (Data.Sync.currentGroupId model.newSiteEntry.securityLevel model.syncData)
                            numberOfKnownDevices
                            model.seed
                      else
                        text "pair a device to save your first password"
                    , Elements.line
                    , lazy (Views.Passwords.view passwordsConfig model.passwordsView) model.syncData
                    , Elements.line
                    , Elements.button (Just ResetDevice) "Reset Device"
                    ]
                )
            ]
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


newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> String -> Int -> Seed -> Element Msg
newSiteForm requirementsState expandSiteEntry entry currentGroupId maxSecurityLevel seed =
    column []
        [ column []
            [ Elements.inputWithLabel (Just SiteNameChanged) "New Site: " "example.com" entry.siteName
            ]
        , (if not expandSiteEntry then
            empty
           else
            column []
                ([ Elements.inputWithLabel (Just UserNameChanged) "Login name" "" entry.userName
                 , Elements.text "Security Level: "
                 , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                 , Views.PasswordGenerator.view (AddPassword currentGroupId) NewPasswordRequirements requirementsState
                 ]
                )
          )
        ]
