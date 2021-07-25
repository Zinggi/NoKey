module Murmur3 exposing (hashString)

{-| Murmur 3 hash function for hashing strings
@docs hashString
-}

import Bitwise exposing (..)
import UTF8


type alias HashData =
    { shift : Int
    , seed : Int
    , hash : Int
    , charsProcessed : Int
    }


{-| Takes a seed and a string. Produces a hash (integer).
Given the same seed and string, it will always produce the same hash.
    hashString 1234 "Turn me into a hash" == 4138100590
-}
hashString : Int -> String -> Int
hashString seed str =
    str
        |> UTF8.foldl hashFold (HashData 0 seed 0 0)
        |> finalize


hashFold : Int -> HashData -> HashData
hashFold c data =
    let
        res =
            c
                |> shiftLeftBy data.shift
                |> or data.hash
    in
        -- Using case-of instead of == avoids costly .cmp check
        case data.shift of
            24 ->
                let
                    newHash =
                        res
                            |> mix data.seed
                            |> step
                in
                    { shift = 0
                    , seed = newHash
                    , hash = 0
                    , charsProcessed = data.charsProcessed + 1
                    }

            _ ->
                { shift = data.shift + 8
                , seed = data.seed
                , hash = res
                , charsProcessed = data.charsProcessed + 1
                }


finalize : HashData -> Int
finalize data =
    let
        acc =
            if data.hash /= 0 then
                mix data.seed data.hash
            else
                data.seed

        h1 =
            Bitwise.xor acc data.charsProcessed

        h2 =
            h1
                |> shiftRightZfBy 16
                |> Bitwise.xor h1
                |> mur 0x85EBCA6B

        h3 =
            h2
                |> shiftRightZfBy 13
                |> Bitwise.xor h2
                |> mur 0xC2B2AE35
    in
        h3
            |> shiftRightZfBy 16
            |> Bitwise.xor h3
            |> shiftRightZfBy 0


mix : Int -> Int -> Int
mix h1 h2 =
    let
        k1 =
            mur 0xCC9E2D51 h2
    in
        k1
            |> shiftLeftBy 15
            |> or (shiftRightZfBy 17 k1)
            |> mur 0x1B873593
            |> Bitwise.xor h1


mur : Int -> Int -> Int
mur c h =
    and 0xFFFFFFFF ((and h 0xFFFF * c) + shiftLeftBy 16 (and 0xFFFF (shiftRightZfBy 16 h * c)))


step : Int -> Int
step acc =
    let
        h1 =
            shiftLeftBy 13 acc
                |> or (shiftRightZfBy 19 acc)
                |> mur 5
    in
        (and h1 0xFFFF + 0x6B64) + shiftLeftBy 16 (and 0xFFFF (shiftRightZfBy 16 h1 + 0xE654))