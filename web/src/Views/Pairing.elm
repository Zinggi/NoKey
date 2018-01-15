module Views.Pairing exposing (State, Config, view, receivedToken, init, tokenSubmitted, pairingCompleted, getTockenClicked)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onSubmit, onClick, onInput)
import RemoteData exposing (WebData, RemoteData(..))
import Http
import QRCode


type alias State =
    { token : WebData String
    , pairingCodeInput : String
    , inputToken : String
    , tokenSubmitStatus : SubmitStatus
    }


init : State
init =
    { pairingCodeInput = "", token = NotAsked, inputToken = "", tokenSubmitStatus = Initial }


type alias Config msg =
    { onGetTokenClicked : msg
    , onSubmitToken : msg
    , toMsg : State -> msg
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


getTockenClicked : State -> State
getTockenClicked s =
    { s | token = Loading, tokenSubmitStatus = Initial }


type SubmitStatus
    = Initial
    | Submitted
    | Answer (Result Http.Error String)


tokenSubmitted : State -> State
tokenSubmitted s =
    { s | tokenSubmitStatus = Submitted }


pairingCompleted : Result Http.Error String -> State -> State
pairingCompleted a s =
    { s | tokenSubmitStatus = Answer a }


view : Config msg -> Bool -> State -> Html msg
view config doShow diag =
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
        if doShow then
            -- TODO: Scan QR code, I could probably use https://github.com/felipenmoura/qr-code-scanner
            Html.div []
                [ Html.button [ onClick config.onGetTokenClicked ] [ Html.text "Get code" ]
                , case ( diag.token, diag.tokenSubmitStatus ) of
                    ( _, Submitted ) ->
                        inp True [ Html.text "wait for response.." ]

                    ( _, Answer (Ok a) ) ->
                        inp True [ Html.text ("Successfully paired with: " ++ a) ]

                    ( _, Answer (Err e) ) ->
                        inp True [ Html.text ("Error: " ++ toString e) ]

                    ( NotAsked, _ ) ->
                        inp True []

                    ( Loading, _ ) ->
                        inp False [ Html.text "wait for token..." ]

                    ( Failure e, _ ) ->
                        inp True [ Html.text ("Something went wrong: " ++ toString e) ]

                    ( Success t, _ ) ->
                        inp True
                            [ Html.div [] [ Html.text "Scan the QR code or type the words shown below." ]
                            , Html.div []
                                [ QRCode.encode t
                                    |> Result.map QRCode.toSvg
                                    |> Result.withDefault
                                        (Html.text "Error while encoding to QRCode.")
                                ]
                            , Html.div [] [ Html.span [] [ Html.text t ] ]
                            ]
                ]
        else
            Html.text ""
