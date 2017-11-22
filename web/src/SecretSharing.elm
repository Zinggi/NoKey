module SecretSharing exposing (..)

import BigInt exposing (BigInt)
import String.UTF8 as UTF8
import Random.Pcg as Random exposing (Generator, Seed)


--

import FiniteField exposing (Field, makeField, primeBiggerThan, secretPolynom, getPolynomialPoints, lagrangeInterpolation)


type alias Share =
    { requiredParts : Int, x : Int, y : BigInt, prime : BigInt }


type alias Secret =
    BigInt


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


splitSecret : ( Int, Int ) -> Secret -> Seed -> ( List Share, Seed )
splitSecret ( level, numPoints ) secret seed =
    let
        prime =
            primeBiggerThan secret

        ( coefficients, newSeed ) =
            Random.step (secretPolynom prime secret (level - 1)) seed

        field =
            makeField prime
    in
        ( getPolynomialPoints field coefficients numPoints
            |> List.map (\( x, y ) -> { requiredParts = level, x = x, y = y, prime = prime })
        , newSeed
        )


joinSecret : List Share -> Result String Secret
joinSecret shares =
    let
        points =
            List.map (\s -> ( BigInt.fromInt s.x, s.y )) shares
    in
        case shares of
            s :: otherShares ->
                Ok (lagrangeInterpolation (makeField s.prime) points (BigInt.fromInt 0))

            _ ->
                Err "Empty list of shares"
