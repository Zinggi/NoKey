module Views.Notifications exposing (view, init, Config, State)

import Set
import Element exposing (..)
import Elements
import Styles
import Data.Notifications as Notifications exposing (..)
import Data.Sync exposing (SyncData)


type alias State =
    Int


type alias Config msg =
    { onRejectRequest : Id -> ShareRequest -> msg
    , onGrantRequest : Id -> ShareRequest -> msg
    , onDismiss : Id -> msg
    , onSaveEntry : Id -> String -> SiteEntry -> msg
    , toMsg : State -> msg
    }


init : State
init =
    2


view : Config msg -> SyncData -> Notifications -> Int -> State -> Element msg
view config sync ns maxSecurityLevel state =
    case Notifications.first ns of
        Just n ->
            viewEntry config sync maxSecurityLevel n state

        Nothing ->
            empty


viewEntry : Config msg -> SyncData -> Int -> Notification -> State -> Element msg
viewEntry config sync maxSecurityLevel n state =
    (case n.data of
        ShareRequestT req ->
            [ column []
                [ Elements.avatar [] (Data.Sync.getDevice req.deviceId sync)
                , Elements.p "wants to unlock"
                , row [] (Elements.enumeration (Elements.groupIcon True) (Set.toList req.keys))
                ]
            , Elements.buttonRow []
                [ Elements.button (Just (config.onRejectRequest n.id req)) "Disallow"
                , Elements.primaryButton (Just (config.onGrantRequest n.id req)) "Allow"
                ]
            ]

        ExternalSiteEntry entry isNew ->
            [ Elements.inputGroup
                (if isNew then
                    "Save this site?"
                 else
                    "Save updated password?"
                )
                [ Elements.inputText Nothing { label = "Site", placeholder = "" } entry.site
                , Elements.inputText Nothing { label = "Login", placeholder = "" } entry.login
                , Elements.passwordEntry Nothing "Password" False entry.password
                , Elements.clampedNumberInput config.toMsg "Security Level" ( 2, 2, maxSecurityLevel ) state
                ]
            , Elements.buttonRow []
                [ Elements.button
                    (Just
                        (config.onSaveEntry n.id
                            (Data.Sync.currentGroupId state sync)
                            { entry | securityLevel = min maxSecurityLevel state }
                        )
                    )
                    "Save"
                , Elements.button (Just (config.onDismiss n.id)) "Forget"
                ]
            ]
    )
        |> column [ height shrink, width fill, padding (Styles.paddingScale 3), spacing (Styles.paddingScale 2) ]
