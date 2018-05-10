module Views.CreateKeyBox exposing (view, State, init, Config)

import Elements
import Element exposing (..)
import Styles


type alias State =
    { password : String
    , passwordAgain : String
    , boxName : String
    }


init : State
init =
    { password = "", passwordAgain = "", boxName = "" }


type alias Config msg =
    { toMsg : State -> msg, onCancel : msg, onCreateKeyBox : { password : String, name : String } -> msg }


entryError : State -> Maybe String
entryError { password, passwordAgain, boxName } =
    -- TODO!: has to be unique!
    if boxName == "" then
        Just "Give a name to your box"
    else if password == "" then
        Just "Password can't be empty"
    else if password /= passwordAgain then
        Just "Passwords do not match"
    else if String.length password < 8 then
        Just "Password should be at least 8 characters"
    else
        Nothing


view : Config msg -> m -> State -> Element msg
view config model state =
    let
        cancelBtn =
            Elements.button (Just config.onCancel) "Cancel"
    in
        column [ spacing (Styles.paddingScale 2) ]
            [ Elements.inputText []
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
            , case entryError state of
                Just err ->
                    column [ spacing (Styles.paddingScale 2) ]
                        [ Elements.p err
                        , cancelBtn
                        ]

                Nothing ->
                    row []
                        [ cancelBtn
                        , Elements.primaryButton
                            (Just (config.onCreateKeyBox { password = state.password, name = state.boxName }))
                            "Create key box"
                        ]
            ]
