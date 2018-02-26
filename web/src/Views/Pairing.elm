module Views.Pairing exposing (State, Config, view, receivedToken, init, tokenSubmitted, pairingCompleted, getTockenClicked)

-- import Html exposing (Html)
-- import Html.Attributes as Attr
-- import Html.Events exposing (onSubmit, onClick, onInput)

import Element exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (Error(..))
import QRCode
import Elements
import Helper exposing (boolToMaybe)
import Styles


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


view : Config msg -> Bool -> State -> Element msg
view config doShow diag =
    let
        inp isEnabled rest =
            column []
                (row [ spacing (Styles.paddingScale 1), Elements.onEnter config.onSubmitToken ]
                    [ Elements.textInput (boolToMaybe isEnabled (\s -> config.toMsg (update (SetInput s) diag))) "enter token" diag.inputToken
                    , Elements.button (boolToMaybe isEnabled config.onSubmitToken) "submit"
                    ]
                    :: rest
                )
    in
        if doShow then
            -- TODO: Scan QR code, I could probably use https://github.com/felipenmoura/qr-code-scanner
            column []
                [ Elements.button (Just config.onGetTokenClicked) "Get code"
                , case ( diag.token, diag.tokenSubmitStatus ) of
                    ( _, Submitted ) ->
                        inp True [ Elements.p "Wait for token.." ]

                    ( _, Answer (Ok a) ) ->
                        inp True [ Elements.p ("Wait on confirmation from: " ++ a) ]

                    ( _, Answer (Err e) ) ->
                        inp True [ Elements.p ("Error: " ++ toString e) ]

                    ( NotAsked, _ ) ->
                        inp True []

                    ( Loading, _ ) ->
                        inp False [ Elements.p "Wait for token.." ]

                    ( Failure e, _ ) ->
                        case e of
                            NetworkError ->
                                inp True [ Elements.p "Either you or the pairing server are offline. Sorry about that :(" ]

                            _ ->
                                inp True [ Elements.p ("Something went wrong: " ++ toString e) ]

                    ( Success t, _ ) ->
                        inp True
                            [ column [] [ Elements.p "Scan the QR code or type the words shown below." ]
                            , column []
                                [ QRCode.encode t
                                    |> Result.map (QRCode.toSvg >> html)
                                    |> Result.withDefault
                                        (Elements.p "Error while encoding to QRCode.")
                                ]
                            , column [] [ Elements.text t ]
                            ]
                ]
        else
            empty
