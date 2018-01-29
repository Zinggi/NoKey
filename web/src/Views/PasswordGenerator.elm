module Views.PasswordGenerator exposing (State, init, view, getNextPassword, getRequirements)

-- import Html exposing (Html)
-- import Html.Attributes as Attr
-- import Html.Events exposing (onCheck, onInput)

import Dict exposing (Dict)
import Random.Pcg.Extended as Random exposing (Seed)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Html
import Html.Attributes as Attr


--

import PasswordGenerator as PG exposing (PasswordRequirements)
import CharSet exposing (CharSet)
import Interval
import Helper exposing (..)
import Elements
import Styles


type alias State =
    { allowedSets : Select
    , atLeastOneOf : Select
    , includeCustom : String
    , excludeCustom : String
    , custom : String
    }


type Msg
    = SetIncludeCustom String
    | SetExcludeCustom String
    | SetCustom String
    | SetAllowed String Bool
    | SetAtLeastOneOff String Bool


update : Msg -> State -> State
update msg state =
    case msg of
        SetIncludeCustom s ->
            { state | includeCustom = s }

        SetExcludeCustom s ->
            { state | excludeCustom = s }

        SetCustom s ->
            { state | custom = s }

        SetAllowed s b ->
            { state | allowedSets = setSet s b state.allowedSets }

        SetAtLeastOneOff s b ->
            { state | atLeastOneOf = setSet s b state.atLeastOneOf }


setSet key b set =
    Dict.update key (Maybe.map (\( _, s ) -> ( b, s ))) set


isSelected key select =
    Dict.get key select
        |> Maybe.map (\( b, s ) -> b)
        |> Maybe.withDefault False


getRequirements : State -> PasswordRequirements
getRequirements state =
    { forbidden = getForbidden state.allowedSets state.includeCustom state.excludeCustom
    , atLeastOneOf = CharSet.fromString state.custom :: filterDict state.atLeastOneOf
    }



-- type Msg =
--     ToggleDetails
--     | CheckToggle Bool String CharSet


type alias Select =
    Dict String ( Bool, CharSet )


init : State
init =
    let
        mkSet b =
            Dict.map (\key s -> ( b, s )) CharSet.commonCharSets
    in
        { allowedSets = mkSet True, atLeastOneOf = mkSet False, includeCustom = "", excludeCustom = "", custom = "" }


getNextPassword : State -> Int -> Seed -> ( Result String String, Seed )
getNextPassword reqs length =
    Random.step (PG.randomPassword length (getRequirements reqs))


getForbidden sets include exclude =
    Interval.unionIntervalList
        (CharSet.fromString exclude)
        (filterDict (Dict.map (\k ( b, s ) -> ( not b, s )) sets)
            |> Interval.union
            |> Interval.subtract (CharSet.fromString include)
        )



-- View


view : (State -> msg) -> State -> Element msg
view toMsg state =
    column
        Styles.background
        [ allowedChars state
        , atLeastOneOf state

        -- , text (toString (getRequirements state))
        ]
        |> Element.map (\msg -> update msg state |> toMsg)


allowedChars state =
    Elements.inputGroup "Allowed Characters"
        [ toggleSets SetAllowed state.allowedSets
        , Elements.input SetIncludeCustom "Include custom" state.includeCustom
        , Elements.input SetExcludeCustom "Exclude custom" state.excludeCustom
        ]


atLeastOneOf state =
    Elements.inputGroup "At least one of"
        [ toggleSets SetAtLeastOneOff state.atLeastOneOf, Elements.input SetCustom "Custom" state.custom ]


toggleSets toMsg set =
    row [] (List.map (\l -> setBox toMsg l set) [ "A-Z", "a-z", "0-9", "!\"#$%…" ])


setBox toMsg label set =
    Elements.checkBox (toMsg label) label (isSelected label set)


{-| TODO: Remove, for dev only
-}
main =
    Html.beginnerProgram { view = view identity >> Element.layout [], update = (\msg model -> msg), model = init }
