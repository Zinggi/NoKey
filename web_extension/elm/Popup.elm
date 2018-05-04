module Popup exposing (main)

import Background
import Model
import MainView
import ExternalStateView
import Element
import Elements
import Styles
import Html exposing (Html)


viewError : String -> Html msg
viewError err =
    (case err of
        "not loaded yet" ->
            Element.empty

        other ->
            Elements.p other
    )
        |> Element.layout Styles.background


main =
    ExternalStateView.program
        { view = MainView.view
        , viewError = viewError
        , subs = Background.subs
        , decodeModel = Model.decode
        , encodeMsg = Model.encodeMsg
        }
