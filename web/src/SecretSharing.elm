module SecretSharing exposing (..)

import BigInt exposing (BigInt)
import String.UTF8 as UTF8
import Random.Pcg as Random exposing (Generator)


--

import FiniteField exposing (Prime)


type alias Share =
    { requiredParts : Int, x : Int, y : BigInt }


type alias Secret =
    BigInt


secretPolynom : Prime -> BigInt -> Int -> Generator (List BigInt)
secretPolynom p c0 numOfCoeffs =
    Random.list numOfCoeffs (bigIntMax p)
        |> Random.map (\x -> c0 :: x)


bigIntMax : BigInt -> Generator BigInt
bigIntMax m =
    let
        l =
            BigInt.toString m |> String.length
    in
        bigIntDigits l
            |> Random.andThen
                (\n ->
                    if BigInt.lt n m then
                        Random.constant n
                    else
                        -- if it doesn meet requirements, try again
                        -- this should terminate in max ~10 rounds, e.g. if m = 11111
                        -- then then the chance of producing a 0 as first char is 1/10
                        bigIntMax m
                )


bigIntDigits : Int -> Generator BigInt
bigIntDigits n =
    Random.list n (Random.int 0 9)
        |> Random.map
            (List.map toString
                >> String.join ""
                >> BigInt.fromString
                >> Maybe.withDefault (BigInt.fromInt 0)
            )


stringToBigInt : String -> BigInt
stringToBigInt s =
    UTF8.toBytes s
        |> List.foldl (\elm acc -> BigInt.add (BigInt.mul acc (BigInt.fromInt 256)) (BigInt.fromInt elm)) (BigInt.fromInt 0)


bigIntToString : BigInt -> String
bigIntToString n =
    bigIntToStringHelp n []
        |> UTF8.toString
        |> Result.withDefault ""


bigIntToInt : BigInt -> Int
bigIntToInt n =
    BigInt.toString n
        |> String.toInt
        |> Result.withDefault 0


bigIntToStringHelp : BigInt -> List Int -> List Int
bigIntToStringHelp n acc =
    if BigInt.gt n (BigInt.fromInt 0) then
        case BigInt.divmod n (BigInt.fromInt 256) of
            Just ( val, digit ) ->
                bigIntToStringHelp val ((bigIntToInt digit) :: acc)

            Nothing ->
                Debug.crash "256 shouldn't be 0 ..."
    else
        acc


splitSecret : ( Int, Int ) -> Secret -> List Share
splitSecret a =
    Debug.crash "todo"


joinSecret : List Share -> Secret
joinSecret a =
    Debug.crash "todo"
