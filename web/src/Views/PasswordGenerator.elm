module Views.PasswordGenerator exposing (State, init, view, nextPassword)

-- import Html exposing (Html)
-- import Html.Attributes as Attr
-- import Html.Events exposing (onCheck, onInput)

import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Html
import Random.Pcg.Extended as Random exposing (Seed)


--

import Views.PasswordRequirements as PwReq
import Elements
import Styles


type alias State =
    { showMore : Bool
    , length : Int
    , requirements : PwReq.State
    , seed : Seed
    }


type Msg
    = PwReq PwReq.State
    | SetIsOpen Bool
    | UpdateLength Int
    | NextPw
    | Reset


update : Msg -> State -> State
update msg state =
    case msg of
        PwReq s ->
            { state | requirements = s }

        UpdateLength i ->
            { state | length = i }

        SetIsOpen b ->
            { state | showMore = b }

        NextPw ->
            nextPassword state

        Reset ->
            { state | requirements = PwReq.init }


init : Seed -> State
init seed =
    { showMore = False, requirements = PwReq.init, length = 16, seed = Random.step Random.independentSeed seed |> Tuple.first }


nextPassword : State -> State
nextPassword state =
    { state | seed = PwReq.getNextPassword state.length state.requirements state.seed |> Tuple.second }



-- View


view : (String -> msg) -> (State -> msg) -> State -> Element msg
view onAcceptPw toMsg state =
    let
        ( isOk, pw, error ) =
            case PwReq.getNextPassword state.length state.requirements state.seed |> Tuple.first of
                Ok p ->
                    ( True, p, "" )

                Err e ->
                    ( False, "", e )
    in
        Elements.container
            [ row []
                [ el [ alignLeft ] <|
                    if isOk then
                        Elements.wrapInBorder (Elements.h3 pw)
                    else
                        Elements.text error
                ]
            , Elements.inputGroup "Length"
                [ row []
                    [ Elements.clampedNumberInput UpdateLength ( 6, 16, 32 ) state.length
                        |> Element.map (\msg -> update msg state |> toMsg)
                    , el [ alignRight ]
                        (row [ spacing (Styles.scaled 1) ] <|
                            if isOk then
                                [ Elements.button (Just NextPw) "Next"
                                    |> Element.map (\msg -> update msg state |> toMsg)
                                , Elements.button (Just (onAcceptPw pw)) "Ok"
                                ]
                            else
                                [ Elements.button (Just Reset) "Reset"
                                    |> Element.map (\msg -> update msg state |> toMsg)
                                ]
                        )
                    ]
                ]
            , Elements.toggleMoreButton SetIsOpen "show more" "show less" state.showMore
                |> Element.map (\msg -> update msg state |> toMsg)
            , if state.showMore then
                PwReq.view PwReq state.requirements
                    |> Element.map (\msg -> update msg state |> toMsg)
              else
                empty
            ]
