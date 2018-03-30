module Main exposing (main)

import Html
import Background
import MainView
import Model exposing (ModelState, Msg, Flags)


main : Program Flags ModelState Msg
main =
    Html.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , view = MainView.view
        , update = Background.updateModelState
        }
