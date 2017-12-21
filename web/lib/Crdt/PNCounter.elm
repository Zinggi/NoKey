module Crdt.PNCounter exposing (PNCounter, init, add, merge, get)

import Crdt.GCounter as GCounter exposing (GCounter)


type alias PNCounter =
    { up : GCounter, down : GCounter }


init : PNCounter
init =
    PNCounter GCounter.init GCounter.init


{-| Add something to the counter. Negative numberes are allowed

    init |> add "A" 3 |> add "A" -2 |> get
    --> 1

-}
add : String -> Int -> PNCounter -> PNCounter
add id n counter =
    if n >= 0 then
        { counter | up = GCounter.add id n counter.up }
    else
        { counter | down = GCounter.add id (-n) counter.down }


{-| merge a remote counter into the local counter

    let
        local = init |> add "A" 4
        remote = init |> add "B" -3
        newLocal = merge local remote
    in
        get newLocal --> 1

-}
merge : PNCounter -> PNCounter -> PNCounter
merge otherCounter myCounter =
    { myCounter | up = GCounter.merge myCounter.up otherCounter.up, down = GCounter.merge myCounter.down otherCounter.down }


get : PNCounter -> Int
get counter =
    GCounter.get counter.up - GCounter.get counter.down
