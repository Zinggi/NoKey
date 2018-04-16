module Views.Dashboard exposing (view, needsPairingHint, hints)

import Dict
import Element exposing (..)
import Elements
import Route
import Model exposing (Msg(..), Model)
import Data.Sync
import Route exposing (Page(..))
import Styles
import Views.Passwords


view : Views.Passwords.Config Msg -> Model -> Element Msg
view passwordsConfig model =
    let
        h =
            hints model

        myId =
            model.uniqueIdentifyier

        knownIds =
            Data.Sync.knownDevices model.syncData
    in
        -- + tasks from passwords
        -- + Shortcut for group status & to unlock a group
        column [ spacing (Styles.scaled 1), height shrink ]
            [ Elements.myAvatar SetDeviceName myId (Dict.get myId knownIds |> Maybe.withDefault ( "", "" )) []
            , viewSummery model
            , Maybe.withDefault empty h.tutorial
            , Maybe.withDefault empty h.needsPairing
            , Views.Passwords.tasks passwordsConfig model.passwordsView (Data.Sync.getTasks model.syncData)
            ]


type alias Hints =
    { tutorial : Maybe (Element Msg)
    , needsPairing : Maybe (Element Msg)
    }


hints : Model -> Hints
hints model =
    -- TODO: show dynamic hints, such as if there is a group with level 2 and you only have two devices,
    -- warn user to pair more.
    { tutorial =
        if model.isFirstTimeUser then
            Just viewTutorial
        else
            Nothing
    , needsPairing = needsPairingHint model
    }


needsPairingHint : { a | syncData : Data.Sync.SyncData } -> Maybe (Element Msg)
needsPairingHint { syncData } =
    if Dict.size (Data.Sync.knownIds syncData) <= 1 then
        Just viewPairingHint
    else
        Nothing


viewTutorial =
    Elements.card 1
        []
        [ Elements.h3 "Welcome!"
        , Elements.p "It seems like this is the first time you're using NoKey. Do you want to view a short tutorial?"
        , row []
            [ Elements.primaryButton (Just (NavigateTo Tutorial)) "Show Tutorial"
            , Elements.button (Just DoneWithTutorial) "No, I've used it before"
            ]
        ]


viewPairingHint =
    Elements.card 1
        []
        [ Elements.text "Pair your first device to get started"
        , Elements.button (Just (NavigateTo Pairing)) "Pair a device"
        ]


viewSummery model =
    -- TODO: show this in the header
    -- TODO: show things like number of (recent) devices online
    column []
        [ row []
            [ Elements.b "Devices"
            , Elements.text (toString (Dict.size (Data.Sync.knownIds model.syncData)))
            ]
        ]
