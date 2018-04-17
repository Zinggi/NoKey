module MainBackground exposing (main)

import Background
import Navigation
import Model exposing (ModelState, Msg, Flags)
import Html


main : Program Flags ModelState Msg
main =
    Navigation.programWithFlags Background.locationToMsg
        { init = Background.init
        , subscriptions = Background.subs
        , update = Background.updateModelState
        , view = \_ -> Html.text ""
        }
