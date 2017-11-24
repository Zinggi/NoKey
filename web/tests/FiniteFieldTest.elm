module FiniteFieldTest exposing (..)

import BigInt
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string, tuple, intRange, string)
import Test exposing (..)


--

import FiniteField exposing (..)
import TestUtils exposing (..)


suite =
    describe "FiniteField"
        [ fuzz int "poly(0) should equal c0" <|
            \c0 ->
                let
                    prime =
                        BigInt.fromInt 31

                    c00 =
                        BigInt.mod (BigInt.fromInt c0) prime
                in
                    evalPolynom (makeField prime) [ c00, BigInt.mod (BigInt.fromInt 42) prime ] (BigInt.fromInt 0)
                        |> Expect.equal c00
        , test "modInverse" <|
            \_ ->
                (makeField (BigInt.fromInt 7)).modInverse (BigInt.fromInt 3)
                    |> Expect.equal (BigInt.fromInt 5)
        , fuzz (bigIntModNonZero (BigInt.fromInt 31)) "x * (modeInverse x base) % base == 1" <|
            \x ->
                let
                    f =
                        makeField (BigInt.fromInt 31)
                in
                    f.mul (f.modInverse x) x
                        |> Expect.equal (BigInt.fromInt 1)
        , test "lagrangeInterpolation on a line" <|
            \_ ->
                let
                    prime =
                        BigInt.fromInt 31

                    points =
                        [ ( BigInt.fromInt 0, BigInt.fromInt 5 ), ( BigInt.fromInt 2, BigInt.fromInt 11 ) ]
                in
                    lagrangeInterpolation (makeField prime) points (BigInt.fromInt 1)
                        |> Expect.equal (BigInt.fromInt 8)
        , test "powMod 42 0 7 == 1" <|
            \_ ->
                (makeField (BigInt.fromInt 7)).pow (BigInt.fromInt 42) (BigInt.fromInt 0)
                    |> Expect.equal (BigInt.fromInt 1)
        , test "powMod 3 3 7 == (3^3) % 7" <|
            \_ ->
                (makeField (BigInt.fromInt 7)).pow (BigInt.fromInt 3) (BigInt.fromInt 3)
                    |> Expect.equal (BigInt.fromInt ((3 ^ 3) % 7))
        ]
