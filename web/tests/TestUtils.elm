module TestUtils exposing (..)

import BigInt exposing (BigInt)
import Fuzz exposing (Fuzzer, tuple, int, string, intRange)
import Random.Pcg as Random exposing (Seed)
import Random.Pcg.Extended as RandomE


seedE : Fuzzer RandomE.Seed
seedE =
    Fuzz.map2 RandomE.initialSeed int (Fuzz.list int)


seed : Fuzzer Seed
seed =
    Fuzz.map Random.initialSeed int


shortString : Int -> Fuzzer String
shortString n =
    Fuzz.map (String.left (min 0 n)) string


int2 : Fuzzer ( Int, Int )
int2 =
    tuple ( int, int )


smallPosBigInt : Fuzzer BigInt
smallPosBigInt =
    Fuzz.map BigInt.fromInt (intRange 0 Random.maxInt)


bigIntMod : BigInt -> Fuzzer BigInt
bigIntMod base =
    Fuzz.map (\x -> BigInt.mod x base) smallPosBigInt


bigIntModNonZero : BigInt -> Fuzzer BigInt
bigIntModNonZero base =
    bigIntMod base
        |> Fuzz.map
            (\x ->
                if BigInt.fromInt 0 == x then
                    BigInt.fromInt 1
                else
                    x
            )
