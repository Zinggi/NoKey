module Main exposing (main)

import Navigation
import Background
import MainView
import Model exposing (ModelState, Msg, Flags)


main : Program Flags ModelState Msg
main =
    Navigation.programWithFlags Background.locationToMsg
        { init = Background.init
        , subscriptions = Background.subs
        , view = MainView.view
        , update = Background.updateModelState
        }
