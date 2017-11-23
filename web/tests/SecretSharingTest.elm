module SecretSharingTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string, tuple, intRange, string)
import Test exposing (..)
import BigInt exposing (BigInt)
import Random.Pcg as Random


--

import SecretSharing
import TestUtils exposing (..)


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
        [ fuzz3 numShares smallPosBigInt seed "secret should be recoverable from minimum number of shares" <|
            \( m, n ) s seed_ ->
                SecretSharing.splitSecret ( m, n ) s seed_
                    |> Tuple.first
                    |> List.take m
                    |> SecretSharing.joinSecret
                    |> Expect.equal (Ok s)
        ]
