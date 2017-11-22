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
    skip <|
        describe "SecretSharing"
            [ fuzz3 numShares smallPosBigInt seed "secret should be recoverable from minimum number of shares" <|
                \( m, n ) s seed_ ->
                    let
                        _ =
                            Debug.log "((m, n), s)" ( ( m, n ), s )
                    in
                        SecretSharing.splitSecret ( m, n ) s seed_
                            |> Debug.log "splits"
                            |> Tuple.first
                            |> List.take m
                            |> Debug.log "first m"
                            |> SecretSharing.joinSecret
                            |> Debug.log "joined"
                            |> Expect.equal (Ok s)
            ]
