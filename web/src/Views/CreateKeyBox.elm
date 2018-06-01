module Views.CreateKeyBox exposing (view, State, init, clear, Config)

import Elements
import Element exposing (..)
import Styles
import Data.Sync exposing (SyncData)
import Data.KeyBox as Boxes exposing (KeyBoxes)


type alias State =
    { password : String
    , passwordAgain : String
    , boxName : String
    }


init : State
init =
    { password = "", passwordAgain = "", boxName = "" }


clear : State -> State
clear s =
    init


type alias Config msg =
    { toMsg : State -> msg, onCancel : msg, onCreateKeyBox : { password : String, name : String, itterations : Int } -> msg }


entryError : KeyBoxes -> State -> Maybe String
entryError boxes { password, passwordAgain, boxName } =
    if boxName == "" then
        Just "Give a name to your box"
    else if password == "" then
        Just "Password can't be empty"
    else if String.length password < 8 then
        Just "Password should be at least 8 characters"
    else if password /= passwordAgain then
        Just "Passwords do not match"
    else if Boxes.isNameTaken boxName boxes then
        Just ("A box with name " ++ boxName ++ " already exists.")
    else
        Nothing


view : Config msg -> { m | syncData : SyncData } -> State -> Element msg
view config model state =
    let
        cancelBtn =
            Elements.button (Just config.onCancel) "Cancel"
    in
        column [ spacing (Styles.paddingScale 2) ]
            [ Elements.paragraph [ spacing (Styles.paddingScale 1) ]
                [ Elements.text """A key box is kinda like a safe that holds a key.
            It allows you to unlock a group even if you don't have enough devices with you.
            The safe is protected by the password you chose below.
            Therefore it is important to """
                , Elements.b "choose a good password!"
                ]
            , el [ height (px (Styles.paddingScale 3)) ] none
            , Elements.inputText []
                (Just (\s -> config.toMsg { state | boxName = s }))
                { label = "Box name", placeholder = "Name your box" }
                state.boxName
            , Elements.newPasswordInput []
                (Just (\s -> config.toMsg { state | password = s }))
                { label = "Password", placeholder = "" }
                state.password
            , Elements.newPasswordInput []
                (Just (\s -> config.toMsg { state | passwordAgain = s }))
                { label = "Password again", placeholder = "" }
                state.passwordAgain
            , case entryError (Data.Sync.getKeyBoxes model.syncData) state of
                Just err ->
                    column [ spacing (Styles.paddingScale 2) ]
                        [ Elements.p err
                        , cancelBtn
                        ]

                Nothing ->
                    row []
                        [ cancelBtn
                        , Elements.primaryButton
                            (Just (config.onCreateKeyBox { password = state.password, name = state.boxName, itterations = Boxes.numItterations }))
                            "Create key box"
                        ]
            ]
