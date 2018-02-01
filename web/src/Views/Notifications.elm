module Views.Notifications exposing (view)

-- import Html exposing (..)
-- import Html.Events exposing (..)

import Element exposing (..)
import Elements
import Data.Notifications as Notifications exposing (..)


view : (Id -> msg) -> (Id -> ShareRequest -> msg) -> Notifications -> Element msg
view onRejectRequest onGrantRequest ns =
    column [] (Notifications.map (viewEntry onRejectRequest onGrantRequest) ns)


viewEntry onRejectRequest onGrantRequest id n =
    case n of
        ShareRequestT req ->
            column []
                [ column [] [ Elements.h4 req.id ]
                , Elements.text (" wants to view password for: " ++ toString req.key)
                , column []
                    [ Elements.button (Just (onRejectRequest id)) "Reject"
                    , Elements.button (Just (onGrantRequest id req)) "Grant"
                    ]
                ]
