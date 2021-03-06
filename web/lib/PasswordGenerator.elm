module PasswordGenerator exposing (..)

import Random.Pcg.Extended as Random exposing (Generator)


--

import Interval
import CharSet exposing (CharSet)
import Helper


type alias PasswordRequirements =
    { forbidden : CharSet
    , atLeastOneOf : List CharSet
    }


standardRequirements : PasswordRequirements
standardRequirements =
    { forbidden = [], atLeastOneOf = [] }


simpleRandomPassword : Int -> CharSet -> Generator (Result String String)
simpleRandomPassword length allowedSymbols =
    CharSet.sampleRandom allowedSymbols
        |> Random.list length
        |> Random.map (\res -> Helper.combineResults res |> Result.map String.fromList)


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
            Helper.sampleSubset l (List.range 0 (length - 1))

        -- sample l random chars
        requiredChars =
            List.map CharSet.sampleRandom atLeastOneOf
                |> Helper.flatten
                |> Random.map Helper.combineResults

        -- generate a random password
        randomPw =
            simpleRandomPassword length allowedSymbols
    in
        Random.map3
            (\pw indices chars ->
                -- combine the random password and replace characters at the sampled locations with their random char
                Result.map2 (\pws cs -> Helper.replaceIndices (List.map2 (,) indices cs) pws) pw chars
            )
            randomPw
            subSet
            requiredChars
