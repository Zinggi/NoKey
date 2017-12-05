module Crdt.GSet exposing (GSet, add, merge, init, get)

import Set exposing (Set)


type alias GSet comparable =
    Set comparable


init : GSet comparable
init =
    Set.empty


get : GSet comparable -> Set comparable
get =
    identity


add : comparable -> GSet comparable -> GSet comparable
add =
    Set.insert


merge : GSet comparable -> GSet comparable -> GSet comparable
merge =
    Set.union
