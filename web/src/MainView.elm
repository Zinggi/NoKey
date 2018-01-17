module MainView exposing (view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Lazy exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Keyed
import Random.Pcg.Extended as Random exposing (Seed)
import SecretSharing


--

import Helper exposing (..)
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
        div []
            [ viewDevices model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
            , Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
            , hr [] []
            , Views.Notifications.view RejectShareRequest GrantShareRequest model.notifications
            , hr [] []
            , if numberOfKnownDevices >= 2 then
                newSiteForm model.requirementsState model.expandSiteEntry model.newSiteEntry numberOfKnownDevices model.seed
              else
                text "pair a device to save your first password"
            , hr [] []
            , lazy2 viewSavedSites model.sitesState model.syncData
            , hr [] []
            , div [] [ button [ onClick ResetDevice ] [ text "Reset Device" ] ]
            ]


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


viewDevices : String -> Dict String String -> Html Msg
viewDevices myId knownIds =
    table []
        (tr [] [ th [] [ text "name" ], th [] [ text "uuid" ] ]
            -- TODO. see if there is a reliable method for detecting online devices
            -- , th [] [ text "status" ] ]
            :: devicesMap (viewDeviceEntry myId) knownIds
        )


{-| TODO: fix input lag on input fields. Workaround:

    https://github.com/elm-lang/html/issues/105#issuecomment-309524197
    https://ellie-app.com/3fPSxX6VHK7a1/0

-}
viewDeviceEntry : String -> String -> String -> Html Msg
viewDeviceEntry myId uuid name =
    tr []
        ([ td []
            [ if myId == uuid then
                input [ value name, onInput SetDeviceName, placeholder "Name your device.." ] []
              else
                text name
            ]
         , td [] [ text uuid ]
         ]
            ++ (if myId /= uuid then
                    [ button [ onClick (RemoveDevice uuid) ] [ text "Remove!" ] ]
                else
                    []
               )
        )


viewSavedSites : RequestPassword.State -> SyncData -> Html Msg
viewSavedSites sitesState sync =
    Data.Sync.mapSavedSites (viewSavedSite sitesState) sync
        |> Html.Keyed.node "div" []


viewSavedSite : RequestPassword.State -> String -> String -> Int -> Maybe SecretSharing.Share -> ( String, Html Msg )
viewSavedSite sitesState siteName userName requiredParts mayShare =
    div []
        [ div [] [ b [] [ text siteName ] ]
        , div []
            [ text userName
            , text (" -> has share: " ++ toString (Nothing /= mayShare))
            , div []
                [ case Dict.get ( siteName, userName ) sitesState of
                    Just recShares ->
                        let
                            shares =
                                maybeToList mayShare ++ recShares
                        in
                            if (List.length shares) >= requiredParts then
                                -- expensive operation
                                text ("Password: " ++ toString (SecretSharing.joinToString shares))
                            else
                                text <| "Received " ++ toString (List.length shares) ++ "/" ++ toString requiredParts ++ " shares"

                    Nothing ->
                        button [ onClick (RequestPasswordPressed ( siteName, userName )) ] [ text "Request password" ]
                ]
            ]
        ]
        |> (\html -> ( siteName ++ userName, html ))


clampedNumberInput : (Int -> msg) -> ( Int, Int, Int ) -> Int -> Html msg
clampedNumberInput toMsg ( min, default, max ) n =
    let
        m =
            clamp min max n
    in
        input
            [ type_ "number"
            , Attr.min (toString min)
            , Attr.max (toString max)
            , value (toString m)
            , onInput (\s -> String.toInt s |> Result.map (clamp min max) |> Result.withDefault default |> toMsg)
            ]
            []


newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> Int -> Seed -> Html Msg
newSiteForm requirementsState expandSiteEntry entry maxSecurityLevel seed =
    let
        pw =
            Tuple.first (Views.PasswordGenerator.getNextPassword requirementsState entry.length seed)
    in
        div []
            [ Html.form [ onSubmit GenerateNewPassword ]
                [ text "New Site: "
                , input [ placeholder "example.com", value entry.siteName, onInput SiteNameChanged ] []
                ]
            , (if not expandSiteEntry then
                text ""
               else
                div []
                    ([ text "Login name: "
                     , input [ value entry.userName, onInput UserNameChanged ] []
                     , text "Security Level: "

                     -- TODO: limit max by number of available devices.
                     , clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                     , text "Password length: "
                     , clampedNumberInput PasswordLengthChanged ( 4, 16, 512 ) entry.length
                     , Views.PasswordGenerator.view NewPasswordRequirements requirementsState
                     ]
                        ++ case pw of
                            Ok thePw ->
                                [ text "your new password: "
                                , text thePw
                                , div
                                    []
                                    [ button [ onClick (AddPassword thePw) ] [ text "OK" ]
                                    , button [ onClick GenerateNewPassword ] [ text "Generate another one!" ]
                                    ]
                                ]

                            Err e ->
                                [ text e ]
                    )
              )
            ]
