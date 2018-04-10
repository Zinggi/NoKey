module Views.Options exposing (view)

import Element exposing (..)
import Elements
import Route
import Model exposing (Msg(..), Model)
import Data.Sync


view : Model -> Element Msg
view model =
    column []
        [ Elements.button (Just ResetDevice) "Reset Device"
        , Elements.line
        , Elements.text ("Version: " ++ Data.Sync.appVersion)
        ]
