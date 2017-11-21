module IntervalTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


--

import Interval exposing (..)
import TestUtils exposing (..)


suite : Test
suite =
    describe "Interval"
        [ fuzz2 int2 int2 "union argument order to Interval.union shouldn't matter" <|
            \a b ->
                intervalUnion (fromTuple a) (fromTuple b)
                    |> Expect.equal (intervalUnion (fromTuple b) (fromTuple a))
        ]
