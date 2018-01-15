module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onSubmit, onClick)
import Html.Lazy exposing (lazy, lazy2)
import Html.Keyed
import Dict exposing (Dict)
import Random.Pcg.Extended as RandomE


--

import Helper exposing (boolToInt, maybeToList, noCmd, withCmds, addCmds)
import Data.Sync exposing (SyncData)
import Data.RequestPassword as RequestPassword
import Views.Notifications
import Views.Pairing
import SecretSharing
import Views.PasswordGenerator as PW
import Data.PasswordMeta exposing (PasswordMetaData)
import Background exposing (Model, Flags, Msg(..))


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
        Html.div []
            [ viewDevices model.uniqueIdentifyier (Data.Sync.knownDevices model.syncData)
            , Views.Pairing.view pairingConfig model.showPairingDialogue model.pairingDialogue
            , Html.hr [] []
            , Views.Notifications.view RejectShareRequest GrantShareRequest model.notifications
            , Html.hr [] []
            , if numberOfKnownDevices >= 2 then
                newSiteForm model.requirementsState model.expandSiteEntry model.newSiteEntry numberOfKnownDevices model.seed
              else
                Html.text "pair a device to save your first password"
            , Html.hr [] []
            , lazy2 viewSavedSites model.sitesState model.syncData
            , Html.hr [] []
            , Html.div [] [ Html.button [ onClick ResetDevice ] [ Html.text "Reset Device" ] ]
            ]


pairingConfig : Views.Pairing.Config Msg
pairingConfig =
    { onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


viewDevices : String -> Dict String String -> Html Msg
viewDevices myId knownIds =
    Html.table []
        (Html.tr [] [ Html.th [] [ Html.text "name" ], Html.th [] [ Html.text "uuid" ] ]
            -- TODO. see if there is a reliable method for detecting online devices
            -- , Html.th [] [ Html.text "status" ] ]
            :: devicesMap (viewDeviceEntry myId) knownIds
        )


viewDeviceEntry : String -> String -> String -> Html Msg
viewDeviceEntry myId uuid name =
    Html.tr []
        ([ Html.td []
            [ if myId == uuid then
                Html.input [ Attr.value name, onInput SetDeviceName, Attr.placeholder "Name your device.." ] []
              else
                Html.text name
            ]
         , Html.td [] [ Html.text uuid ]
         ]
            ++ (if myId /= uuid then
                    [ Html.button [ onClick (RemoveDevice uuid) ] [ Html.text "Remove!" ] ]
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
    Html.div []
        [ Html.div [] [ Html.b [] [ Html.text siteName ] ]
        , Html.div []
            [ Html.text userName
            , Html.text (" -> has share: " ++ toString (Nothing /= mayShare))
            , Html.div []
                [ case Dict.get ( siteName, userName ) sitesState of
                    Just recShares ->
                        let
                            shares =
                                maybeToList mayShare ++ recShares
                        in
                            if (List.length shares) >= requiredParts then
                                -- expensive operation
                                Html.text ("Password: " ++ toString (SecretSharing.joinToString shares))
                            else
                                Html.text <| "Received " ++ toString (List.length shares) ++ "/" ++ toString requiredParts ++ " shares"

                    Nothing ->
                        Html.button [ onClick (RequestPasswordPressed ( siteName, userName )) ] [ Html.text "Request password" ]
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
        Html.input
            [ Attr.type_ "number"
            , Attr.min (toString min)
            , Attr.max (toString max)
            , Attr.value (toString m)
            , onInput (\s -> String.toInt s |> Result.map (clamp min max) |> Result.withDefault default |> toMsg)
            ]
            []


newSiteForm : PW.State -> Bool -> PasswordMetaData -> Int -> RandomE.Seed -> Html Msg
newSiteForm requirementsState expandSiteEntry entry maxSecurityLevel seed =
    let
        pw =
            Tuple.first (PW.getNextPassword requirementsState entry.length seed)
    in
        Html.div []
            [ Html.form [ onSubmit GenerateNewPassword ]
                [ Html.text "New Site: "
                , Html.input [ Attr.placeholder "example.com", Attr.value entry.siteName, onInput SiteNameChanged ] []
                ]
            , (if not expandSiteEntry then
                Html.text ""
               else
                Html.div []
                    ([ Html.text "Login name: "
                     , Html.input [ Attr.value entry.userName, onInput UserNameChanged ] []
                     , Html.text "Security Level: "

                     -- TODO: limit max by number of available devices.
                     , clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
                     , Html.text "Password length: "
                     , clampedNumberInput PasswordLengthChanged ( 4, 16, 512 ) entry.length
                     , PW.view NewPasswordRequirements requirementsState
                     ]
                        ++ case pw of
                            Ok thePw ->
                                [ Html.text "your new password: "
                                , Html.text thePw
                                , Html.div
                                    []
                                    [ Html.button [ onClick (AddPassword thePw) ] [ Html.text "OK" ]
                                    , Html.button [ onClick GenerateNewPassword ] [ Html.text "Generate another one!" ]
                                    ]
                                ]

                            Err e ->
                                [ Html.text e ]
                    )
              )
            ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , view = view
        , update = Background.update
        }
