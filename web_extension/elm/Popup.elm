module Popup exposing (main)

import Background
import Model
import MainView
import ExternalStateView


main =
    ExternalStateView.program
        { view = MainView.view
        , subs = Background.subs
        , decodeModel = Model.decode
        , encodeMsg = Model.encodeMsg
        }
