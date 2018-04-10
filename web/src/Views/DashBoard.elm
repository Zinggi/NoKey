module Views.DashBoard exposing (view)

import Dict
import Element exposing (..)
import Elements
import Route
import Model exposing (Msg(..), Model)
import Data.Sync
import Route exposing (Page(..))


view : Model -> Element Msg
view model =
    column []
        [ if model.isFirstTimeUser then
            viewTutorial
          else
            empty
        , viewHints model
        , viewSummery model
        ]


viewTutorial =
    Elements.card []
        [ Elements.h3 "Welcome!"
        , Elements.p "It seems like this is the first time you're using NoKey. Do you want to view a short tutorial?"
        , row []
            [ Elements.primaryButton (Nothing {- Just ShowTutorial -}) "Show Tutorial"
            , Elements.button (Nothing {- Just DoneWithTutorial -}) "No, I've used it before"
            ]
        ]


viewHints model =
    -- TODO: show dynamic hints, such as if there is a group with level 2 and you only have two devices,
    -- warn user to pair more.
    if Dict.size (Data.Sync.knownIds model.syncData) <= 1 then
        column []
            [ Elements.text "Pair your first device to get started"
            , Elements.button (Just (NavigateTo Pairing)) "Pair a device"
            ]
    else
        empty


viewSummery model =
    -- TODO: show things like number of (recent) devices online
    -- + tasks from passwords
    -- + Shortcut for group status & to unlock a group
    column []
        [ row []
            [ Elements.b "Devices"
            , Elements.text (toString (Dict.size (Data.Sync.knownIds model.syncData)))
            ]
        ]
