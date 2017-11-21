module SecretSharingTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string, tuple, intRange, string)
import Test exposing (..)
import BigInt exposing (BigInt)


--

import SecretSharing
import TestUtils exposing (..)


secret : Fuzzer BigInt
secret =
    Fuzz.map BigInt.fromInt int


numShares : Fuzzer ( Int, Int )
numShares =
    tuple ( intRange 1 4, intRange 4 10 )


suite2 : Test
suite2 =
    describe "Utils"
        [ fuzz (shortString 50) "StringToBigInt s |> bigIntToString == s" <|
            \s ->
                SecretSharing.stringToBigInt s
                    |> SecretSharing.bigIntToString
                    |> Expect.equal s
        ]


suite : Test
suite =
    describe "SecretSharing"
        [ fuzz2 numShares secret "secret should be recoverable from minimum number of shares" <|
            \( m, n ) s ->
                SecretSharing.splitSecret ( m, n ) s
                    |> List.take m
                    |> SecretSharing.joinSecret
                    |> Expect.equal s
        ]
