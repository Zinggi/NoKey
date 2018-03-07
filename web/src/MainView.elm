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
import Data.Sync exposing (SyncData, AccountId, GroupId)
import Data.RequestGroupPassword as RequestPassword exposing (Status(..))
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Views.Devices
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
                    , lazy (\( a, b ) -> viewSavedSites a b) ( model.groupPasswordRequestsState, model.syncData )
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


{-| TODO: this should be like in the UI scetch "Passwords"
-}
viewSavedSites : RequestPassword.State -> SyncData -> Element Msg
viewSavedSites sitesState sync =
    Data.Sync.mapAccounts (viewSavedSite sitesState) sync
        |> Element.Keyed.column []


viewSavedSite : RequestPassword.State -> AccountId -> GroupId -> Maybe SecretSharing.Share -> ( String, Element Msg )
viewSavedSite sitesState (( siteName, userName ) as accountId) groupId mayShare =
    column []
        [ column []
            [ Elements.h3 siteName
            , row []
                [ Elements.text userName
                , Elements.text (" -> has share: " ++ toString (Nothing /= mayShare))
                ]
            , row []
                [ case NotRequested {- TODO: was: RequestPassword.getStatusFor accountId sitesState -} of
                    NotRequested ->
                        Elements.button (Just (RequestPasswordPressed groupId Nothing)) "Request password"

                    Waiting n m ->
                        Elements.text ("Received " ++ toString n ++ "/" ++ toString m ++ " shares")

                    Done _ pw ->
                        Elements.text ("Password: " ++ pw)

                    Error error ->
                        Elements.text ("Couldn't recover password, reason:\n" ++ error)
                , Elements.button (Just (DeletePassword ( siteName, userName ))) "Delete"
                ]
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
                 , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                 , Views.PasswordGenerator.view AddPassword NewPasswordRequirements requirementsState
                 ]
                )
          )
        ]
