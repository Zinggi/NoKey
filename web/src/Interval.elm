module Interval exposing (..)

{-|


# Interval
-}

import Set


{-| An interval is specified with the start and end points, both inclusive.
Constraint: for (a, b): a <= b
-}
type alias Interval =
    ( Int, Int )


{-| An interval list is a minimal representation of multiple intervals.
Constraint: sorted from low to high and minimal
-}
type alias IntervalList =
    List Interval


{-| Map over all numbers included in the specified set.

    import Char

    map identity [(1,3), (5,9)]
    --> [1,2,3,5,6,7,8,9]

    map Char.fromCode [(0x30, 0x39), (0x41, 0x5A)]
        |> String.fromList
    --> "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-}
map : (Int -> a) -> IntervalList -> List a
map f list =
    mapHelp f list []


mapHelp : (Int -> a) -> IntervalList -> List a -> List a
mapHelp f list acc =
    case list of
        ( low, high ) :: xs ->
            mapHelp f xs (List.concat [ acc, List.range low high |> List.map f ])

        [] ->
            acc


makeInterval : Int -> Int -> Interval
makeInterval a b =
    if a < b then
        ( a, b )
    else
        ( b, a )


fromTuple : ( Int, Int ) -> Interval
fromTuple =
    uncurry makeInterval


fromTuples : List ( Int, Int ) -> IntervalList
fromTuples list =
    List.map fromTuple list
        |> minimize


{-|

    fromList [5, 7, 1, 1, 34, 2, 7, 5, 6]
    --> [(1, 2), (5, 7), (34, 34)]
-}
fromList : List Int -> IntervalList
fromList list =
    Set.fromList list
        -- remove duplicates and sort
        |> Set.toList
        |> sortedToIntervalList


{-| Converts a sorted list of Ints to an IntervalList
-}
sortedToIntervalList : List Int -> IntervalList
sortedToIntervalList list =
    case list of
        x :: xs ->
            sortedToIntervalListHelp x x xs []
                |> List.reverse

        [] ->
            []


sortedToIntervalListHelp low high list acc =
    case list of
        x :: xs ->
            if x - high == 1 then
                sortedToIntervalListHelp low x xs acc
            else
                sortedToIntervalListHelp x x xs (( low, high ) :: acc)

        [] ->
            ( low, high ) :: acc


{-|

    minimize [(1, 1), (2, 5), (-1, 8), (9, 10), (14, 70)]
    --> [(-1, 10), (14, 70)]
-}
minimize : List Interval -> IntervalList
minimize list =
    List.foldl insert [] list


{-| subtracts the first argument from the second. E.g. subtract a b = b / a.
Intended to be used in a pipe!

    [(1, 5), (8, 10)]
        |> subtract [(2, 3), (5, 8)]
    --> [(1, 1), (4, 4), (9, 10)]

-}
subtract : IntervalList -> IntervalList -> IntervalList
subtract l1 l2 =
    List.foldl removeInterval l2 l1


isEmpty : IntervalList -> Bool
isEmpty l =
    l == []


{-|

    [(1, 5), (8, 10)]
        |> removeInterval (2, 3)
    --> [(1, 1), (4, 5), (8, 10)]

    [(1, 1), (4, 5), (8, 10)]
        |> removeInterval (5, 8)
    --> [(1, 1), (4, 4), (9, 10)]
-}
removeInterval : Interval -> IntervalList -> IntervalList
removeInterval int list =
    case list of
        x :: xs ->
            subtractInterval x int ++ removeInterval int xs

        [] ->
            []


{-|

    subtractInterval (4, 5) (5, 8)
    --> [(4, 4)]

    subtractInterval (1, 6) (4, 10)
    --> [(1, 3)]

    subtractInterval (2, 5) (2, 9)
    --> []

    subtractInterval (2, 5) (100, 105)
    --> [(2, 5)]

    subtractInterval (1, 9) (3, 5)
    --> [(1, 2), (6, 9)]
-}
subtractInterval : Interval -> Interval -> IntervalList
subtractInterval ( l1, h1 ) ( l2, h2 ) =
    let
        ( ( nl1, nh1 ), ( nl2, nh2 ) ) =
            ( ( l1, min (l2 - 1) h1 ), ( max l1 (h2 + 1), h1 ) )
    in
        (if nh1 < nl1 then
            []
         else
            [ ( nl1, nh1 ) ]
        )
            ++ (if nh2 < nl2 then
                    []
                else
                    [ ( nl2, nh2 ) ]
               )


{-|

    insert (1, 6) [(-1, 3), (5, 6), (10, 12)]
    --> [(-1, 6), (10, 12)]

    insert (-100, 100) [(-1, 3), (5, 6), (10, 12)]
    --> [(-100, 100)]

    insert (5, 6) [(-1, 3), (8, 12)]
    --> [(-1, 3), (5, 6), (8, 12)]
-}
insert : Interval -> IntervalList -> IntervalList
insert int list =
    case list of
        x :: xs ->
            case intervalUnion int x of
                [ merged ] ->
                    insert merged xs

                _ ->
                    if x < int then
                        x :: insert int xs
                    else
                        int :: x :: xs

        [] ->
            [ int ]


{-|

    intervalUnion (0, 3) (2, 5) --> [(0, 5)]

    intervalUnion (0, 3) (5, 7) --> [(0, 3), (5, 7)]

    intervalUnion (0, 3) (4, 7) --> [(0, 7)]
-}
intervalUnion : Interval -> Interval -> IntervalList
intervalUnion a b =
    if touches a b then
        [ hull a b ]
    else
        List.sort [ a, b ]


union : List IntervalList -> IntervalList
union lists =
    List.foldl unionIntervalList [] lists


unionIntervalList : IntervalList -> IntervalList -> IntervalList
unionIntervalList l1 l2 =
    List.foldl insert l1 l2


{-| Gets the boundaries of two intervals

    hull (makeInterval 1 5) (makeInterval 2 7)
    --> makeInterval 1 7

    hull (makeInterval 2 5) (makeInterval -3 0)
    --> makeInterval -3 5

-}
hull : Interval -> Interval -> Interval
hull ( l1, h1 ) ( l2, h2 ) =
    ( min l1 l2, max h1 h2 )


{-|

    intersects (makeInterval -2 5) (makeInterval 3 7)
    --> True

    intersects (makeInterval 0 3) (makeInterval 5 8)
    --> False
-}
intersects : Interval -> Interval -> Bool
intersects ( l1, h1 ) ( l2, h2 ) =
    not (l2 > h1 || h2 < l1)


{-| Same as intersects, but also returns true if the boundaries are touching

    touches (makeInterval -2 5) (makeInterval 3 7)
    --> True

    touches (makeInterval 0 3) (makeInterval 5 8)
    --> False

    touches (1, 5) (6, 7)
    --> True

-}
touches : Interval -> Interval -> Bool
touches ( l1, h1 ) ( l2, h2 ) =
    not (l2 > h1 + 1 || h2 + 1 < l1)
