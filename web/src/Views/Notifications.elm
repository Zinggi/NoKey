module Views.Notifications exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Data.Notifications as Notifications exposing (..)


view : (Id -> msg) -> (Id -> ShareRequest -> msg) -> Notifications -> Html msg
view onRejectRequest onGrantRequest ns =
    div [] (Notifications.map (viewEntry onRejectRequest onGrantRequest) ns)


viewEntry onRejectRequest onGrantRequest id n =
    case n of
        ShareRequestT req ->
            div []
                [ div [] [ b [] [ text req.id ], Html.text (" wants to view password for: " ++ toString req.key) ]
                , div []
                    [ button [ onClick (onRejectRequest id) ] [ Html.text "Reject" ]
                    , button [ onClick (onGrantRequest id req) ] [ Html.text "Grant" ]
                    ]
                ]
