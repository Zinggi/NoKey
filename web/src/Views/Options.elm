module Views.Options exposing (view)

import Element exposing (..)
import Elements
import Route exposing (Page(..))
import Model exposing (Msg(..), Model)
import Data.Sync
import Data.Options exposing (Options, setAllowLevel1)
import Styles


view : { m | options : Options } -> Element Msg
view { options } =
    column [ spacing (Styles.paddingScale 2) ]
        [ Elements.button (Just (NavigateTo Tutorial)) "Show Tutorial"
        , Elements.line
        , Elements.text ("Version: " ++ Data.Sync.appVersion)
        , el [ paddingXY 0 (Styles.paddingScale 3) ] (Elements.h3 "Dangerous")

        -- TODO: should show confirm first
        , Elements.checkBox (\b -> setAllowLevel1 b options |> SetOptions) False "Allow security level 1" options.allowLevel1
        , Elements.p allowLevel1Txt
        , Elements.line
        , Elements.button (Just ResetDevice) "Reset Device"
        ]


allowLevel1Txt : String
allowLevel1Txt =
    """
If you select this option, you will be able to save passwords in a group with security level 1.
This is equivalent to no security at all! If someone steals your device, they will be able to read these passwords.
So only use this for storing throwaway accounts, not for anything you care about!
"""
