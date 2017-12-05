module Crdt.ORSet exposing (ORSet, init, add, remove, get, merge)

import Dict exposing (Dict)
import Set exposing (Set)


type alias ORSet comparable =
    Dict comparable ( List Int, List Int )


init : ORSet comparable
init =
    Dict.empty


add : comparable -> ORSet comparable -> ORSet comparable
add e set =
    Dict.update e
        (\mv ->
            case mv of
                Just ( x :: xs, r ) ->
                    Just ( (x + 1 :: x :: xs), r )

                _ ->
                    Just ( [ 1 ], [] )
        )
        set


remove : comparable -> ORSet comparable -> ORSet comparable
remove e set =
    Dict.update e
        (\mv ->
            case mv of
                Just ( adds, removes ) ->
                    Just ( adds, mergeLists adds removes )

                Nothing ->
                    Nothing
        )
        set


{-|

    import Set

    let
        a = init |> add 1 |> add 42
        b = init |> add 42 |> remove 42 |> add 2
    in
        merge a b |> get
        --> Set.fromList [1, 2]
-}
merge : ORSet comparable -> ORSet comparable -> ORSet comparable
merge a b =
    Dict.merge
        (\key a result ->
            Dict.insert key a result
        )
        (\key ( aa, ar ) ( ba, br ) result ->
            Dict.insert key ( (mergeLists aa ba), mergeLists ar br ) result
        )
        (\key b result ->
            Dict.insert key b result
        )
        a
        b
        Dict.empty


{-|

    import Set

    init |> add 1 |> add 2 |> remove 2 |> get
    --> Set.fromList [1]
-}
get : ORSet comparable -> Set comparable
get set =
    Dict.foldl
        (\key ( add, remove ) acc ->
            if Set.isEmpty (Set.diff (Set.fromList add) (Set.fromList remove)) then
                acc
            else
                Set.insert key acc
        )
        Set.empty
        set


{-| merges a sorted list (from high to low) into another sorted list, dropping duplicates
-}
mergeLists : List Int -> List Int -> List Int
mergeLists a b =
    case ( a, b ) of
        ( aa :: aas, bb :: bbs ) ->
            if aa > bb then
                aa :: mergeLists aas b
            else if aa < bb then
                bb :: mergeLists a bbs
            else
                aa :: mergeLists aas bbs

        ( [], bbs ) ->
            bbs

        ( aas, [] ) ->
            aas
