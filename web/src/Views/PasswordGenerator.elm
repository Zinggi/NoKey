module Views.PasswordGenerator exposing (State, init, view, nextPassword)

import Element exposing (..)
import Random.Pcg.Extended as Random exposing (Seed)
import Html.Attributes as Attr


--

import Views.PasswordRequirements as PwReq
import Elements
import Styles
import Helper


type alias State =
    { showMore : Bool
    , length : Int
    , requirements : PwReq.State
    , seed : Seed
    , pw : Maybe String
    , pwAgain : String
    , hasCleared : Bool
    , counter : Int
    }


type Msg
    = PwReq PwReq.State
    | SetIsOpen Bool
    | UpdateLength Int
    | SetPw String
    | SetPwAgain String
    | NextPw
    | Reset


update : Msg -> State -> State
update msg state =
    case msg of
        PwReq s ->
            { state | requirements = s }
                |> nextPassword

        UpdateLength i ->
            { state | length = i } |> nextPassword

        SetIsOpen b ->
            { state | showMore = b }

        SetPw pw ->
            { state | pw = Just pw, hasCleared = state.hasCleared || String.length pw <= 1 }

        SetPwAgain pw ->
            { state | pwAgain = pw }

        NextPw ->
            nextPassword state

        Reset ->
            nextPassword { state | requirements = PwReq.init }


init : Seed -> State
init seed =
    { showMore = False
    , requirements = PwReq.init
    , length = 16
    , seed = Random.step Random.independentSeed seed |> Tuple.first
    , pw = Nothing
    , pwAgain = ""
    , counter = 0
    , hasCleared = False
    }


nextPassword : State -> State
nextPassword state =
    { state
        | seed = PwReq.getNextPassword state.length state.requirements state.seed |> Tuple.second
        , pw = Nothing
        , pwAgain = ""
        , counter = state.counter + 1
        , hasCleared = False
    }



-- View


view : (String -> msg) -> Bool -> (State -> msg) -> State -> Element msg
view onAcceptPw canAdd toMsg state =
    let
        ( isOk, pass, error ) =
            case PwReq.getNextPassword state.length state.requirements state.seed |> Tuple.first of
                Ok p ->
                    ( True, p, "" )

                Err e ->
                    ( False, "", e )

        pw =
            case state.pw of
                Just p ->
                    p

                Nothing ->
                    pass

        isManuallyEntering =
            state.pw /= Nothing && state.hasCleared

        inp onIn type_ label =
            Elements.inputTextHackHelper (toString state.counter ++ label) type_ [] [] (Just onIn) { label = label, placeholder = "" }

        entryValid =
            not isManuallyEntering || pw == state.pwAgain
    in
        column [ spacing (Styles.paddingScale 3) ]
            [ if isOk then
                (if not isManuallyEntering then
                    column [] [ inp SetPw "text" "Password" pw ]
                 else
                    column [ spacing (Styles.paddingScale 3) ]
                        [ inp SetPw "password" "Password" pw
                        , inp SetPwAgain "password" "Password again" state.pwAgain
                        ]
                )
                    |> Element.map (\msg -> update msg state |> toMsg)
              else
                Elements.p error
            , column []
                [ if isManuallyEntering then
                    none
                  else
                    Elements.clampedNumberInput UpdateLength "Length" ( 6, 16, 32 ) state.length
                        |> Element.map (\msg -> update msg state |> toMsg)
                , row [ spacing (Styles.scaled 1) ] <|
                    if isOk then
                        [ Elements.button (Just NextPw) "Next"
                            |> Element.map (\msg -> update msg state |> toMsg)
                        , Elements.primaryButton (Helper.boolToMaybe (canAdd && entryValid) (onAcceptPw pw)) "Ok"
                        ]
                    else
                        [ Elements.primaryButton (Just Reset) "Reset"
                            |> Element.map (\msg -> update msg state |> toMsg)
                        ]
                ]
            , if isManuallyEntering then
                none
              else
                Elements.toggleMoreButton SetIsOpen "show more" "show less" state.showMore
                    |> Element.map (\msg -> update msg state |> toMsg)
            , if state.showMore && not isManuallyEntering then
                PwReq.view PwReq state.requirements
                    |> Element.map (\msg -> update msg state |> toMsg)
              else
                none
            ]
