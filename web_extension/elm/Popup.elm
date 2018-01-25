module Popup exposing (main)

import Background
import MainView
import ExternalStateView


main =
    ExternalStateView.program
        { view = MainView.view
        , subs = Background.subs
        , decodeModel = Background.decodeModel
        , encodeMsg = Background.encodeMsg
        }
