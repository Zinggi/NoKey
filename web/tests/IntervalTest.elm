module IntervalTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string, tuple)
import Test exposing (..)


--

import Interval exposing (..)


int2 =
    tuple ( int, int )


suite : Test
suite =
    fuzz2 int2 int2 "union argument order to Interval.union shouldn't matter" <|
        \a b ->
            intervalUnion (fromTuple a) (fromTuple b)
                |> Expect.equal (intervalUnion (fromTuple b) (fromTuple a))
