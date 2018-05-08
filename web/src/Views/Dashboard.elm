module Views.Dashboard exposing (view, needsPairingHint, hints)

import Dict
import Element exposing (..)
import Elements
import Route
import Model exposing (Msg(..), Model)
import Data.Sync
import Data.Settings exposing (Settings)
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
            , Maybe.withDefault none h.tutorial
            , Tuple.second h.needsPairing
            , Views.Passwords.tasks passwordsConfig model.passwordsView (Data.Sync.getTasks model.syncData)
            , if Tuple.first h.needsPairing then
                none
              else
                column [ spacing (Styles.scaled 1) ]
                    [ pairButton
                    , addPasswordButton
                    ]
            ]


type alias Hints =
    { tutorial : Maybe (Element Msg)
    , needsPairing : ( Bool, Element Msg )
    }


hints : Model -> Hints
hints model =
    { tutorial =
        if model.isFirstTimeUser then
            Just viewTutorial
        else
            Nothing
    , needsPairing = needsPairingHint model
    }


{-| The bool indicates if it should be rendered inside the passwords view
-}
needsPairingHint : { a | syncData : Data.Sync.SyncData } -> ( Bool, Element Msg )
needsPairingHint { syncData } =
    let
        ( numDev, minSec, maxLevel ) =
            ( Data.Sync.numberOfKnownDevices syncData
            , Data.Sync.minSecurityLevel syncData
            , Data.Sync.maxUsedSecurityLevel syncData
            )
    in
        if numDev < minSec then
            ( True, viewPairingHint )
        else if numDev == maxLevel then
            ( False, viewPairMoreHint numDev maxLevel )
            -- TODO: add hint for when we lose access to a group.
            -- this could offer solutions for recovery, maybe we made a backup somewhere?
        else
            ( False, none )


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


viewPairMoreHint numDev maxLevel =
    Elements.card 1
        []
        [ Elements.b "Pair at least one more device"
        , Elements.p
            ("If you would lose a device now, you would lose access to the passwords saved with security level "
                ++ toString maxLevel
                ++ ", since you only have "
                ++ toString numDev
                ++ " paired devices."
            )
        , pairButton
        ]


viewPairingHint =
    Elements.card 1
        []
        [ Elements.p "Pair your first device to get started"
        , pairButton
        ]


pairButton =
    Elements.button (Just (NavigateTo Pairing)) "Pair a device"


addPasswordButton =
    Elements.button (Just (NavigateTo NewPassword)) "Add a new password"


viewSummery model =
    -- TODO: show this in the header
    -- TODO: show things like number of (recent) devices online
    let
        numDev =
            Data.Sync.numberOfKnownDevices model.syncData
    in
        if numDev > 1 then
            column []
                [ row [ spacing (Styles.paddingScale 2) ]
                    [ Elements.text (toString numDev)
                    , Elements.b "Devices"
                    ]
                ]
        else
            none
