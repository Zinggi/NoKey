module CharSet exposing (..)

import Dict exposing (Dict)
import Char
import Random.Pcg.Extended as Random exposing (Generator)
import Interval exposing (IntervalList)


type alias CharSet =
    IntervalList


ascii =
    [ ( 0x20, 0x7E ) ]


uppercase =
    [ ( 0x41, 0x5A ) ]


lowercase =
    [ ( 0x61, 0x7A ) ]


numbers =
    [ ( 0x30, 0x39 ) ]


specialChars =
    Interval.fromTuples ascii
        |> Interval.subtract (List.concat [ numbers, uppercase, lowercase ] |> Interval.fromTuples)


commonCharSets : Dict String CharSet
commonCharSets =
    Dict.fromList
        [ ( "numbers", numbers )
        , ( "lowercase", lowercase )
        , ( "uppercase", uppercase )
        , ( "specialChars", specialChars )
        ]


fromString : String -> CharSet
fromString str =
    String.toList str
        |> List.map Char.toCode
        |> Interval.fromList


sampleRandom : CharSet -> Generator (Result String Char)
sampleRandom l =
    Random.sample (Interval.map Char.fromCode l)
        |> Random.map
            (\maybeChar ->
                case maybeChar of
                    Nothing ->
                        Err "empty set of allowed characters!"

                    Just c ->
                        Ok c
            )
