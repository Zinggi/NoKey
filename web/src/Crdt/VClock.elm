module Crdt.VClock exposing (VClock, PartialOrder(..), init, increment, compare, merge)

import Dict exposing (Dict)


type alias VClock =
    Dict String Int


init : VClock
init =
    Dict.empty


increment : String -> VClock -> VClock
increment id =
    Dict.update id
        (\mval ->
            case mval of
                Just v ->
                    Just (v + 1)

                Nothing ->
                    Just 1
        )


type PartialOrder
    = Before
    | After
    | Concurrent
    | Equal


mergeOrder : PartialOrder -> PartialOrder -> PartialOrder
mergeOrder a b =
    case ( a, b ) of
        ( Concurrent, _ ) ->
            Concurrent

        ( _, Concurrent ) ->
            Concurrent

        ( Before, After ) ->
            Concurrent

        ( After, Before ) ->
            Concurrent

        ( Before, beforeOrEqual ) ->
            Before

        ( After, afterOrEqual ) ->
            After

        ( Equal, afterBeforeOrEqual ) ->
            afterBeforeOrEqual


compToOrder : Int -> Int -> PartialOrder
compToOrder a b =
    if a < b then
        Before
    else if a == b then
        Equal
    else
        After


{-| compare a b
Equal -> (a == b)
Before -> (a < b)
After -> (a > b)
Concurrent -> we don't know

    Crdt.VClock.compare (init |> increment "A" |> increment "B") (init |> increment "A")
    --> After

    Crdt.VClock.compare (init |> increment "A") (init |> increment "B")
    --> Concurrent

-}
compare : VClock -> VClock -> PartialOrder
compare aC bC =
    Dict.merge
        (\key a result ->
            mergeOrder After result
        )
        (\key a b result ->
            mergeOrder (compToOrder a b) result
        )
        (\key b result ->
            mergeOrder Before result
        )
        aC
        bC
        Equal


merge : VClock -> VClock -> VClock
merge aC bC =
    Dict.merge
        (\key a result ->
            Dict.insert key a result
        )
        (\key a b result ->
            Dict.insert key (max a b) result
        )
        (\key b result ->
            Dict.insert key b result
        )
        aC
        bC
        Dict.empty
