module MainView exposing (view)

import Dict exposing (Dict)
import Html exposing (Html)
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
import Data.RequestPassword as RequestPassword
import Data.PasswordMeta exposing (PasswordMetaData)
import Views.Pairing
import Views.Notifications
import Views.PasswordGenerator
import Background exposing (Model, Msg(..))


devicesMap : (String -> String -> b) -> Dict String String -> List b
devicesMap f known_ids =
    Dict.foldl
        (\uuid name acc ->
            f uuid name :: acc
        )
        []
        known_ids


view : Model -> Html Msg
view model =
    let
        numberOfKnownDevices =
            Data.Sync.knownIds model.syncData |> List.length
    in
        column []
            [ viewDevices model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
            , Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
            , Elements.line
            , Views.Notifications.view RejectShareRequest GrantShareRequest model.notifications
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
            |> Element.layout Styles.background


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


viewDevices : String -> Dict String String -> Element Msg
viewDevices myId knownIds =
    Elements.table [ ( "name", .name ), ( "uuid", .uuid ) ]
        (devicesMap (viewDeviceEntry myId) knownIds)


{-| TODO: fix input lag on input fields. Workaround:

    https://github.com/elm-lang/html/issues/105#issuecomment-309524197
    https://ellie-app.com/3fPSxX6VHK7a1/0

-}
viewDeviceEntry : String -> String -> String -> { name : Element Msg, uuid : Element Msg }
viewDeviceEntry myId uuid name =
    { name =
        if myId == uuid then
            Elements.textInput SetDeviceName "Name your device.." name
        else
            Elements.text name
    , uuid =
        row [] <|
            Elements.text uuid
                :: (if myId /= uuid then
                        [ Elements.button (Just (RemoveDevice uuid)) "Remove!" ]
                    else
                        []
                   )
    }


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
            , case Dict.get ( siteName, userName ) sitesState of
                Just recShares ->
                    let
                        shares =
                            maybeToList mayShare ++ recShares
                    in
                        if (List.length shares) >= requiredParts then
                            -- expensive operation
                            Elements.text ("Password: " ++ toString (SecretSharing.joinToString shares))
                        else
                            Elements.text <| "Received " ++ toString (List.length shares) ++ "/" ++ toString requiredParts ++ " shares"

                Nothing ->
                    Elements.button (Just (RequestPasswordPressed ( siteName, userName ))) "Request password"
            ]
        ]
        |> (\elem -> ( siteName ++ userName, elem ))


newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> Int -> Seed -> Element Msg
newSiteForm requirementsState expandSiteEntry entry maxSecurityLevel seed =
    column []
        [ column []
            -- TODO
            -- Html.form [ onSubmit GenerateNewPassword ]
            [ Elements.input SiteNameChanged "New Site: " "example.com" entry.siteName
            ]
        , (if not expandSiteEntry then
            empty
           else
            column []
                ([ Elements.input UserNameChanged "Login name" "" entry.userName
                 , Elements.text "Security Level: "

                 -- TODO: limit max by number of available devices.
                 , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                 , Views.PasswordGenerator.view AddPassword NewPasswordRequirements requirementsState
                 ]
                )
          )
        ]
