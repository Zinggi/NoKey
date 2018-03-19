module Main exposing (..)

import Html exposing (Html)


--

import Background
import MainView


main =
    Html.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , view = MainView.view
        , update = Background.updateModelState
        }
