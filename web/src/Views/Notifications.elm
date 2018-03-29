module Views.Notifications exposing (view, init, Config, State)

-- import Html exposing (..)
-- import Html.Events exposing (..)

import Set
import Element exposing (..)
import Elements
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
    case n.data of
        ShareRequestT req ->
            column []
                [ column []
                    [ Elements.avatar [] (Data.Sync.getDevice req.deviceId sync)
                    , Elements.p "wants to view password for"
                    , row [] (Elements.enumeration (Elements.groupIcon True) (Set.toList req.keys))
                    ]
                , row []
                    [ Elements.button (Just (config.onRejectRequest n.id req)) "Reject"
                    , Elements.primaryButton (Just (config.onGrantRequest n.id req)) "Grant"
                    ]
                ]

        ExternalSiteEntry entry isNew ->
            column []
                [ Elements.inputGroup
                    (if isNew then
                        "Save this site?"
                     else
                        "Save updated password?"
                    )
                    [ Elements.inputWithLabel Nothing "Site" "" entry.site
                    , Elements.inputWithLabel Nothing "Login" "" entry.login
                    , Elements.passwordEntry Nothing "Password" False entry.password
                    , Elements.withLabel "Security Level" <| Elements.clampedNumberInput config.toMsg ( 2, 2, maxSecurityLevel ) state
                    ]
                , Elements.buttonRow []
                    [ Elements.button (Just (config.onSaveEntry n.id (Data.Sync.currentGroupId state sync) { entry | securityLevel = min maxSecurityLevel state })) "Save"
                    , Elements.button (Just (config.onDismiss n.id)) "Forget"
                    ]
                ]
