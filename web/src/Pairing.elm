module Pairing exposing (State, Config, view, receivedToken, init)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onSubmit, onClick, onInput)
import RemoteData exposing (WebData, RemoteData(..))


type alias State =
    { token : WebData String
    , pairingCodeInput : String
    , inputToken : String
    }


init : State
init =
    { pairingCodeInput = "", token = NotAsked, inputToken = "" }


type alias Config msg =
    { onGetTokenClicked : msg
    , onSubmitToken : msg
    , toMsg : State -> msg
    , doShow : Bool
    }


type Msg
    = SetInput String


update : Msg -> State -> State
update msg state =
    case msg of
        SetInput s ->
            { state | inputToken = s }


receivedToken : WebData String -> State -> State
receivedToken token state =
    { state | token = token }


view : Config msg -> State -> Html msg
view config diag =
    let
        inp isEnabled rest =
            Html.form [ onSubmit config.onSubmitToken ]
                ([ Html.input
                    [ Attr.placeholder "enter token"
                    , onInput (\s -> config.toMsg (update (SetInput s) diag))
                    , Attr.value diag.inputToken
                    , Attr.disabled (not isEnabled)
                    ]
                    []
                 ]
                    ++ rest
                )
    in
        if config.doShow then
            -- TODO: display QR code
            -- probably using: pablohirafuji/elm-qrcode
            Html.div []
                [ Html.button [ onClick config.onGetTokenClicked ] [ Html.text "Get code" ]
                , case diag.token of
                    NotAsked ->
                        inp True []

                    Loading ->
                        inp False [ Html.text "wait for token..." ]

                    Failure e ->
                        inp True [ Html.text ("Something went wrong: " ++ toString e) ]

                    Success t ->
                        Html.div [] [ Html.text "token: ", Html.span [] [ Html.text t ] ]
                ]
        else
            Html.text ""
