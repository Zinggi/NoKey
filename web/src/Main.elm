module Main exposing (..)

import Html exposing (Html)


--

import Background exposing (Model, Flags, Msg(..))
import MainView


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , view = MainView.view
        , update = Background.update
        }
