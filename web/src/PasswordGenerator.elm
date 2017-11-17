module PasswordGenerator exposing (..)

import Random.Pcg as Random exposing (Generator, Seed)


--

import Interval exposing (Interval, IntervalList)
import CharSet exposing (CharSet)
import Helper exposing (..)


type alias PasswordRequirements =
    { forbidden : CharSet
    , atLeastOneOf : List CharSet
    }


standardRequirements =
    { forbidden = [], atLeastOneOf = [] }


{-| TODO: the seed has to come from a secure source for it to be secure.
Plus it should have way bits than PCG uses, e.g. Pcg_32_k64 would be much better.
-}
simpleRandomPassword : Int -> CharSet -> Generator (Result String String)
simpleRandomPassword length allowedSymbols =
    CharSet.sampleRandom allowedSymbols
        |> Random.list length
        |> Random.map (\res -> combineResults res |> Result.map String.fromList)


randomPassword : Int -> PasswordRequirements -> Generator (Result String String)
randomPassword length requirements =
    let
        allowedSymbols =
            CharSet.ascii
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
            List.map CharSet.sampleRandom atLeastOneOf
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
