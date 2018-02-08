port module ExternalStateView exposing (Config, program, sendMsgToBackground, onNewState, getState)

{-| More info here: <https://discourse.elm-lang.org/t/one-background-wroker-multiple-views-of-the-same-state-possible/540>
-}

import Html exposing (Html)
import Json.Encode as JE exposing (Value)


port sendMsgToBackground : Value -> Cmd msg


port onNewState : (Value -> msg) -> Sub msg


port getState : {} -> Cmd msg


type alias Model model =
    Result String model


init : ( Model model, Cmd (Msg msg) )
init =
    ( Err "not loaded yet", getState {} )


type Msg msg
    = NewState Value
    | BackgroundMsg msg


update : (msg -> Value) -> (Value -> Model model) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update encodeMsg decodeModel msg model =
    case msg of
        NewState bgModel ->
            ( decodeModel bgModel, Cmd.none )

        BackgroundMsg bgMsg ->
            ( model, sendMsgToBackground (encodeMsg bgMsg) )


subs : (model -> Sub msg) -> Model model -> Sub (Msg msg)
subs subs model =
    [ Result.map subs model
        |> Result.withDefault Sub.none
        |> Sub.map BackgroundMsg
    , onNewState NewState
    ]
        |> Sub.batch


view : (model -> Html msg) -> Model model -> Html (Msg msg)
view viewFn model =
    case model of
        Ok m ->
            Html.map BackgroundMsg (viewFn m)

        Err e ->
            -- TODO: display loading icon (or nothing?) instead of this
            Html.text e


type alias Config model msg =
    { decodeModel : Value -> Result String model
    , encodeMsg : msg -> Value
    , view : model -> Html msg
    , subs : model -> Sub msg
    }


program : Config model msg -> Program Never (Model model) (Msg msg)
program config =
    Html.program
        { init = init
        , update = update config.encodeMsg config.decodeModel
        , view = view config.view
        , subscriptions = subs config.subs
        }
