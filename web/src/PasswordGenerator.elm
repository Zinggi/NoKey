module PasswordGenerator exposing (..)

import Char
import Random.Pcg as Random exposing (Generator)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onInput)


--

import Interval exposing (Interval, IntervalList)


type alias PasswordRequirements =
    { forbidden : CharSet
    , atLeastOneOf : List CharSet
    }


type alias CharSet =
    IntervalList


commonCharSets : Dict String CharSet
commonCharSets =
    Dict.fromList
        [ ( "numbers", numbers )
        , ( "lowercase", lowercase )
        , ( "uppercase", uppercase )
        , ( "specialChars"
          , Interval.fromTuples asciiSet
                |> Interval.subtract (List.concat [ numbers, uppercase, lowercase ] |> Interval.fromTuples)
          )
        ]


type alias PasswordRequirementsState =
    { forbiddenSets : Dict String ( Bool, CharSet )
    , atLeastOneOf : Dict String ( Bool, CharSet )
    , customForbidden : String
    , customAtLeastOneOf : String
    }


requirementsInitState : PasswordRequirementsState
requirementsInitState =
    let
        cSet =
            Dict.map (\key s -> ( False, s )) commonCharSets
    in
        { forbiddenSets = cSet, customForbidden = "", atLeastOneOf = cSet, customAtLeastOneOf = "" }


viewRequirements : (PasswordRequirementsState -> msg) -> PasswordRequirementsState -> Html msg
viewRequirements toMsg state =
    Html.div []
        [ Html.h3 [] [ Html.text "Password can't contain any of the following:" ]
        , viewCharSets toMsg state
        ]


getRequirements : PasswordRequirementsState -> PasswordRequirements
getRequirements state =
    { forbidden = getForbidden state.forbiddenSets state.customForbidden
    , atLeastOneOf = fromString state.customAtLeastOneOf :: filterDict state.atLeastOneOf
    }


filterDict : Dict comparable ( Bool, a ) -> List a
filterDict sets =
    Dict.toList sets
        |> List.filterMap
            (\( k, ( b, s ) ) ->
                if b then
                    Just s
                else
                    Nothing
            )


getForbidden sets customForbidden =
    Interval.unionIntervalList (fromString customForbidden) (Interval.union <| filterDict sets)


viewCharSets : (PasswordRequirementsState -> msg) -> PasswordRequirementsState -> Html msg
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


standardRequirements =
    { forbidden = [], atLeastOneOf = [] }


fromString : String -> CharSet
fromString str =
    String.toList str
        |> List.map Char.toCode
        |> Interval.fromList


asciiSet =
    [ ( 0x20, 0x7E ) ]


asciiReducedSet =
    List.concat [ uppercase, lowercase, numbers ]


uppercase =
    [ ( 0x41, 0x5A ) ]


lowercase =
    [ ( 0x61, 0x7A ) ]


numbers =
    [ ( 0x30, 0x39 ) ]


{-| TODO: the seed has to come from a secure source for it to be secure.
Also, the generator should be cryptographically secure, which Random.PCG isn't!!!
-}
simpleRandomPassword : Int -> CharSet -> Generator (Result String String)
simpleRandomPassword length allowedSymbols =
    randomCharInSet allowedSymbols
        |> Random.list length
        |> Random.map (\res -> combineResults res |> Result.map String.fromList)


{-| Combine a list of results into a single result (holding a list).
-}
combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


randomCharInSet : CharSet -> Generator (Result String Char)
randomCharInSet l =
    Random.sample (Interval.map Char.fromCode l)
        |> Random.map
            (\maybeChar ->
                case maybeChar of
                    Nothing ->
                        Err "empty set of allowed characters!"

                    Just c ->
                        Ok c
            )


flatten : List (Generator a) -> Generator (List a)
flatten gens =
    case gens of
        g :: gs ->
            g
                |> Random.andThen
                    (\a ->
                        flatten gs
                            |> Random.map (\aS -> a :: aS)
                    )

        [] ->
            Random.constant []


randomPassword : Int -> PasswordRequirements -> Generator (Result String String)
randomPassword length requirements =
    let
        allowedSymbols =
            asciiSet
                |> Interval.subtract requirements.forbidden

        atLeastOneOf =
            -- filter out impossible requirements, e.g. if a requirement forbidds numbers
            -- and requires at least one number, ignore the atLeastOneOf requirement.
            List.filterMap
                (\reqSet ->
                    let
                        newSet =
                            reqSet |> Interval.subtract requirements.forbidden
                    in
                        if Interval.isEmpty newSet then
                            Nothing
                        else
                            Just newSet
                )
                requirements.atLeastOneOf

        l =
            List.length atLeastOneOf

        -- sample l values out of range 0 length-1
        subSet =
            sampleSubset l (List.range 0 (length - 1))

        -- sample l random chars
        requiredChars =
            List.map randomCharInSet atLeastOneOf
                |> flatten
                |> Random.map combineResults

        -- generate a random password
        randomPw =
            simpleRandomPassword length allowedSymbols
    in
        Random.map3
            (\pw indices chars ->
                -- combine the random password and replace characters at the sampled locations with their random char
                Result.map2 (\pws cs -> replaceIndices (List.map2 (,) indices cs) pws) pw chars
            )
            randomPw
            subSet
            requiredChars


replaceIndices : List ( Int, Char ) -> String -> String
replaceIndices indices s =
    List.foldl (\( i, c ) str -> replaceCharAtIndex i c str) s indices


replaceCharAtIndex : Int -> Char -> String -> String
replaceCharAtIndex i c s =
    indexedMap
        (\ii cc ->
            if i == ii then
                c
            else
                cc
        )
        s


indexedMap : (Int -> Char -> Char) -> String -> String
indexedMap f s =
    String.toList s
        |> List.indexedMap f
        |> String.fromList


{-| Sample a subset of length n of the given list.
-}
sampleSubset : Int -> List a -> Generator (List a)
sampleSubset n set =
    if n <= 0 then
        Random.constant []
    else
        Random.sample set
            |> Random.andThen
                (\maybeElem ->
                    case maybeElem of
                        Nothing ->
                            Random.constant []

                        Just e ->
                            sampleSubset (n - 1) (List.filter (\a -> a /= e) set)
                                |> Random.map (\l -> e :: l)
                )
