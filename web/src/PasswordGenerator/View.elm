module PasswordGenerator.View exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onInput)
import Dict exposing (Dict)
import Random.Pcg as Random exposing (Seed)


--

import PasswordGenerator as PG exposing (PasswordRequirements)
import CharSet exposing (CharSet)
import Interval
import Helper exposing (..)


type alias State =
    { forbiddenSets : Dict String ( Bool, CharSet )
    , atLeastOneOf : Dict String ( Bool, CharSet )
    , customForbidden : String
    , customAtLeastOneOf : String
    }


init : State
init =
    let
        cSet =
            Dict.map (\key s -> ( False, s )) CharSet.commonCharSets
    in
        { forbiddenSets = cSet, customForbidden = "", atLeastOneOf = cSet, customAtLeastOneOf = "" }


view : (State -> msg) -> State -> Html msg
view toMsg state =
    Html.div []
        [ Html.h3 [] [ Html.text "Password can't contain any of the following:" ]
        , viewCharSets toMsg state
        ]


getRequirements : State -> PasswordRequirements
getRequirements state =
    { forbidden = getForbidden state.forbiddenSets state.customForbidden
    , atLeastOneOf = CharSet.fromString state.customAtLeastOneOf :: filterDict state.atLeastOneOf
    }


viewCharSets : (State -> msg) -> State -> Html msg
viewCharSets toMsg ({ forbiddenSets, atLeastOneOf } as state) =
    List.concat
        [ viewSets (\b key set -> toMsg { state | forbiddenSets = Dict.insert key ( b, set ) forbiddenSets }) forbiddenSets
        , [ customSet (\t -> toMsg { state | customForbidden = t }) state.customForbidden ]
        , [ Html.div [] [ Html.text "At least one of these:" ] ]
        , viewSets (\b key set -> toMsg { state | atLeastOneOf = Dict.insert key ( b, set ) atLeastOneOf })
            (Dict.filter (\key v -> Dict.get key forbiddenSets |> Maybe.map (not << Tuple.first) |> Maybe.withDefault False) atLeastOneOf)
        , [ customSet (\t -> toMsg { state | customAtLeastOneOf = t }) state.customAtLeastOneOf ]
        ]
        |> Html.div []


viewSets : (Bool -> String -> CharSet -> msg) -> Dict String ( Bool, CharSet ) -> List (Html msg)
viewSets toMsg sets =
    Dict.toList sets
        |> List.map
            (\( key, ( isChecked, set ) ) ->
                Html.div []
                    [ Html.text key
                    , Html.input
                        [ Attr.type_ "checkbox"
                        , onCheck (\b -> toMsg b key set)
                        , Attr.checked isChecked
                        ]
                        []
                    ]
            )


customSet toMsg set =
    Html.div []
        [ Html.text "Custom: "
        , Html.input
            [ Attr.type_ "text"
            , Attr.value set
            , onInput toMsg
            ]
            []
        ]


getNextPassword : State -> Int -> Seed -> ( Result String String, Seed )
getNextPassword reqs length =
    Random.step (PG.randomPassword length (getRequirements reqs))


getForbidden sets customForbidden =
    Interval.unionIntervalList (CharSet.fromString customForbidden) (Interval.union <| filterDict sets)
