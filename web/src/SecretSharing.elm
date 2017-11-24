module SecretSharing exposing (..)

import BigInt exposing (BigInt)
import Json.Decode as JD
import Json.Encode as JE
import Random.Pcg as Random exposing (Generator, Seed)
import String.UTF8 as UTF8


--

import FiniteField exposing (Field, Prime, makeField, primeBiggerThan, secretPolynom, getPolynomialPoints, lagrangeInterpolation)


type alias Share =
    { requiredParts : Int, x : Int, y : BigInt, prime : BigInt }


shareDecoder : JD.Decoder Share
shareDecoder =
    JD.map4 Share
        (JD.at [ "requiredParts" ] JD.int)
        (JD.at [ "x" ] JD.int)
        (JD.at [ "y" ] bigIntDecoder)
        (JD.at [ "prime" ] bigIntDecoder)


shareToJson : Share -> String
shareToJson s =
    shareEncoder s
        |> JE.encode 4


shareFromJson : String -> Result String Share
shareFromJson =
    JD.decodeString shareDecoder


bigIntDecoder : JD.Decoder BigInt
bigIntDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case BigInt.fromString s of
                    Just i ->
                        JD.succeed i

                    Nothing ->
                        JD.fail "Couldn't convert String to BigInt!"
            )


shareEncoder : Share -> JE.Value
shareEncoder s =
    JE.object
        [ ( "requiredParts", JE.int s.requiredParts )
        , ( "x", JE.int s.x )
        , ( "y", bigIntEncoder s.y )
        , ( "prime", bigIntEncoder s.prime )
        ]


bigIntEncoder : BigInt -> JE.Value
bigIntEncoder i =
    JE.string (BigInt.toString i)


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


splitString : ( Int, Int ) -> String -> Seed -> ( List Share, Seed )
splitString config secret =
    splitSecret config (stringToBigInt secret)


joinToString : List Share -> Result String String
joinToString shares =
    joinSecret shares
        |> Result.map bigIntToString


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


createMoreShares : Int -> List Share -> Result String (List Share)
createMoreShares n existingShares =
    withEnoughShares
        (\requiredParts prime shares ->
            let
                poly x =
                    lagrangeInterpolation (makeField prime) (sharesToPoints shares) (BigInt.fromInt x)
            in
                List.sortBy .x shares
                    |> List.head
                    |> Maybe.map .x
                    |> Maybe.withDefault 1
                    |> (\maxx ->
                            List.range (maxx + 1) (maxx + n)
                                |> List.map (\x -> { requiredParts = requiredParts, x = x, y = poly x, prime = prime })
                       )
        )
        existingShares


sharesToPoints : List Share -> List ( BigInt, BigInt )
sharesToPoints =
    List.map (\s -> ( BigInt.fromInt s.x, s.y ))


{-| Run some function (f requiredParts prime shares) on the the shares if there are enough shares.
Otherwise returns an error.
-}
withEnoughShares : (Int -> Prime -> List Share -> a) -> List Share -> Result String a
withEnoughShares f shares =
    case shares of
        s :: otherShares ->
            if List.length shares >= s.requiredParts then
                Ok (f s.requiredParts s.prime shares)
            else
                Err "not enough parts to decrypt the secret"

        _ ->
            Err "Empty list of shares"


joinSecret : List Share -> Result String Secret
joinSecret shares =
    withEnoughShares
        (\_ prime shares ->
            lagrangeInterpolation (makeField prime) (sharesToPoints shares) (BigInt.fromInt 0)
        )
        shares
