module Views.Options exposing (view)

import Element exposing (..)
import Elements
import Route exposing (Page(..))
import Model exposing (Msg(..), Model)
import Data.Sync
import Styles


view : Model -> Element Msg
view model =
    column [ spacing (Styles.paddingScale 1) ]
        [ Elements.button (Just (NavigateTo Tutorial)) "Show Tutorial"
        , Elements.line

        -- TODO: should show confirm first
        , Elements.button (Just ResetDevice) "Reset Device"
        , Elements.line
        , Elements.button (Just (ShowToast "Test toast!")) "Show toast"
        , Elements.line
        , Elements.text ("Version: " ++ Data.Sync.appVersion)
        ]
