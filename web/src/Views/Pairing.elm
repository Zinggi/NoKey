module Views.Pairing
    exposing
        ( State
        , Config
        , view
        , receivedToken
        , init
        , tokenSubmitted
        , pairingCompleted
        , getTockenClicked
        , setInputToken
        )

import Regex
import Element exposing (..)
import Html.Attributes as Attr
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (Error(..))
import Json.Decode as JD
import QRCode
import Elements
import Helper exposing (boolToMaybe)
import Styles


type alias State =
    { token : WebData String
    , inputToken : String
    , tokenSubmitStatus : SubmitStatus
    }


init : State
init =
    { token = NotAsked, inputToken = "", tokenSubmitStatus = Initial }


type alias Config msg =
    { onGetTokenClicked : msg
    , onSubmitToken : msg
    , onScanQR : msg
    , toMsg : State -> msg
    }


type Msg
    = SetInput String


update : Msg -> State -> State
update msg state =
    case msg of
        SetInput s ->
            setInputToken s state


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


setInputToken : String -> State -> State
setInputToken token state =
    { state | inputToken = token }


shouldShowPairButton : String -> Bool
shouldShowPairButton txt =
    Regex.contains (Regex.regex "\\s*\\w+\\s+\\w+\\s+\\w+\\s+\\w+\\s*") txt


view : Config msg -> Bool -> State -> Element msg
view config isAndroid diag =
    let
        inp isEnabled rest =
            column [ spacing (Styles.paddingScale 2) ]
                [ column [] [ Elements.p "Use your other device to scan the QR code of the other device or type the words shown below it" ]
                , row
                    (spacing (Styles.paddingScale 1)
                        :: if shouldShowPairButton diag.inputToken then
                            [ Elements.onEnter config.onSubmitToken ]
                           else
                            []
                    )
                    [ Elements.inputText
                        [ htmlAttribute (Attr.autocomplete False)
                        , htmlAttribute (Attr.attribute "autocorrect" "off")
                        , htmlAttribute (Attr.attribute "autocapitalize" "off")
                        , htmlAttribute (Attr.spellcheck False)
                        ]
                        (boolToMaybe isEnabled (\s -> config.toMsg (update (SetInput s) diag)))
                        { placeholder = "enter token", label = "" }
                        diag.inputToken
                    , if shouldShowPairButton diag.inputToken then
                        Elements.primaryButton (boolToMaybe isEnabled config.onSubmitToken) "Pair"
                      else
                        empty
                    , if isAndroid then
                        Elements.primaryButton (Just config.onScanQR) "Scan"
                      else
                        empty
                    ]
                , column [ width shrink, centerX, spacing (Styles.paddingScale 1) ]
                    (rest ++ [ Elements.button (Just config.onGetTokenClicked) "Get a new token" ])
                ]
    in
        -- TODO: Scan QR code, I could probably use https://github.com/felipenmoura/qr-code-scanner
        column [ height shrink, spacing (Styles.paddingScale 2) ]
            [ case ( diag.token, diag.tokenSubmitStatus ) of
                ( _, Submitted ) ->
                    inp True [ Elements.p "Wait for token.." ]

                ( _, Answer (Ok a) ) ->
                    inp True [ Elements.p ("Wait on confirmation from: " ++ a) ]

                ( _, Answer (Err e) ) ->
                    case e of
                        BadPayload _ { body } ->
                            case JD.decodeString (JD.field "error" JD.string) body of
                                Ok "token expired" ->
                                    inp True [ Elements.p ("Error: Unknown token. You probably made a spelling mistake") ]

                                Ok errStr ->
                                    inp True [ Elements.p ("Error: " ++ errStr) ]

                                _ ->
                                    inp True [ Elements.p ("Error: " ++ toString e) ]

                        _ ->
                            inp True [ Elements.p ("Error: " ++ toString e) ]

                ( NotAsked, _ ) ->
                    inp True []

                ( Loading, _ ) ->
                    inp False [ Elements.p "Wait for token.." ]

                ( Failure e, _ ) ->
                    case e of
                        NetworkError ->
                            inp True
                                [ Elements.p
                                    ("Either you or the pairing server are offline."
                                        ++ " Or you have the wrong server URL. Sorry about that :("
                                    )
                                ]

                        _ ->
                            inp True [ Elements.p ("Something went wrong: " ++ toString e) ]

                ( Success t, _ ) ->
                    let
                        niceT =
                            Helper.replaceString "-" " " t
                    in
                        inp True
                            [ el [ centerX, width shrink ]
                                (QRCode.encode niceT
                                    |> Result.map (QRCode.toSvg >> html)
                                    |> Result.withDefault
                                        (Elements.p "Error while encoding to QRCode.")
                                )
                            , el [ centerX ] (Elements.text niceT)
                            ]
            ]
