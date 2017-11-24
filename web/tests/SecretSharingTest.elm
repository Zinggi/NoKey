module SecretSharingTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string, tuple, intRange, string, tuple3)
import Test exposing (..)


--

import SecretSharing
import TestUtils exposing (..)


numShares : Fuzzer ( Int, Int )
numShares =
    tuple ( intRange 1 4, intRange 4 10 )


share : Fuzzer SecretSharing.Share
share =
    Fuzz.map4
        (\parts x y prime ->
            { requiredParts = parts, x = x, y = y, prime = prime }
        )
        int
        int
        smallPosBigInt
        smallPosBigInt


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
        [ fuzzWith { runs = 20 } (tuple3 ( numShares, smallPosBigInt, seed )) "secret should be recoverable from minimum number of shares" <|
            \( ( m, n ), s, seed_ ) ->
                SecretSharing.splitSecret ( m, n ) s seed_
                    |> Tuple.first
                    |> List.take m
                    |> SecretSharing.joinSecret
                    |> Expect.equal (Ok s)
        , fuzz share "share decoder should be the reverse of encode" <|
            \share ->
                SecretSharing.shareToJson share
                    |> SecretSharing.shareFromJson
                    |> Expect.equal (Ok share)
        ]
