module MainView exposing (view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Element exposing (..)
import Element.Keyed
import Element.Lazy exposing (lazy)
import Random.Pcg.Extended as Random exposing (Seed)
import SecretSharing


--

import Helper exposing (..)
import Styles
import Elements
import Data.Sync exposing (SyncData)
import Data.RequestPassword as RequestPassword exposing (Status(..))
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
import Background exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        numberOfKnownDevices =
            Data.Sync.knownIds model.syncData |> List.length
    in
        row
            [ alignTop ]
            [ column [ attribute (Attr.style [ ( "maxWidth", "800px" ) ]) ]
                (if Notifications.count model.notifications > 0 then
                    [ Views.Notifications.view notificationsConfig model.notifications numberOfKnownDevices model.notificationsView ]
                 else
                    [ Views.Devices.view model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
                    , Elements.line
                    , Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
                    , Elements.line
                    , if numberOfKnownDevices >= 2 then
                        newSiteForm model.requirementsState model.expandSiteEntry model.newSiteEntry numberOfKnownDevices model.seed
                      else
                        text "pair a device to save your first password"
                    , Elements.line
                    , lazy (\( a, b ) -> viewSavedSites a b) ( model.sitesState, model.syncData )
                    , Elements.line
                    , Elements.button (Just ResetDevice) "Reset Device"
                    ]
                )
            ]
            |> Element.layout Styles.background


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


viewSavedSites : RequestPassword.State -> SyncData -> Element Msg
viewSavedSites sitesState sync =
    Data.Sync.mapSavedSites (viewSavedSite sitesState) sync
        |> Element.Keyed.column []


viewSavedSite : RequestPassword.State -> String -> String -> Int -> Maybe SecretSharing.Share -> ( String, Element Msg )
viewSavedSite sitesState siteName userName requiredParts mayShare =
    column []
        [ column []
            [ Elements.h3 siteName
            , row []
                [ Elements.text userName
                , Elements.text (" -> has share: " ++ toString (Nothing /= mayShare))
                ]
            , case RequestPassword.getStatus ( siteName, userName ) sitesState of
                NotRequested ->
                    Elements.button (Just (RequestPasswordPressed ( siteName, userName ) False)) "Request password"

                Waiting n m ->
                    Elements.text ("Received " ++ toString n ++ "/" ++ toString m ++ " shares")

                Done _ pw ->
                    Elements.text ("Password: " ++ pw)

                Error error ->
                    Elements.text ("Couldn't recover password, reason:\n" ++ error)
            ]
        ]
        |> (\elem -> ( siteName ++ userName, elem ))


newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> Int -> Seed -> Element Msg
newSiteForm requirementsState expandSiteEntry entry maxSecurityLevel seed =
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

                 -- TODO: limit max by number of available devices.
                 , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                 , Views.PasswordGenerator.view AddPassword NewPasswordRequirements requirementsState
                 ]
                )
          )
        ]
