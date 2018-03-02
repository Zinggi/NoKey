module Views.Notifications exposing (view, init, Config, State)

-- import Html exposing (..)
-- import Html.Events exposing (..)

import Element exposing (..)
import Elements
import Data.Notifications as Notifications exposing (..)


type alias State =
    Int


type alias Config msg =
    { onRejectRequest : Id -> msg
    , onGrantRequest : Id -> ShareRequest -> msg
    , onDismiss : Id -> msg
    , onSaveEntry : Id -> SiteEntry -> msg
    , toMsg : State -> msg
    }


init : State
init =
    2


view : Config msg -> Notifications -> Int -> State -> Element msg
view config ns maxSecurityLevel state =
    case Notifications.first ns of
        Just n ->
            viewEntry config maxSecurityLevel n state

        Nothing ->
            empty


viewEntry : Config msg -> Int -> Notification -> State -> Element msg
viewEntry config maxSecurityLevel n state =
    case n.data of
        ShareRequestT req ->
            column []
                [ column [] [ Elements.h4 req.id ]
                , -- TODO: create a view for this, e.g. Id -> Icon + name/partOfId
                  row []
                    [ Elements.hashIcon req.id
                    , Elements.text (" wants to view password for: " ++ toString req.key)
                    ]
                , column []
                    [ Elements.button (Just (config.onRejectRequest n.id)) "Reject"
                    , Elements.button (Just (config.onGrantRequest n.id req)) "Grant"
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
                    , Elements.passwordEntry Nothing "Password" entry.password
                    , Elements.withLabel "Security Level" <| Elements.clampedNumberInput config.toMsg ( 2, 2, maxSecurityLevel ) state
                    ]
                , Elements.buttonRow []
                    [ Elements.button (Just (config.onSaveEntry n.id { entry | securityLevel = min maxSecurityLevel state })) "Save"
                    , Elements.button (Just (config.onDismiss n.id)) "Forget"
                    ]
                ]
