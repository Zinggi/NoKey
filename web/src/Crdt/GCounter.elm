module Crdt.GCounter exposing (GCounter, init, add, merge, get, decoder, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Dict exposing (Dict)


type alias GCounter =
    Dict String Int


init : GCounter
init =
    Dict.empty


encode : GCounter -> Value
encode =
    JE.dict identity JE.int


decoder : Decoder GCounter
decoder =
    JD.dict JD.int


{-| add something to the counter. Negative numberes are interpreted as 0

    init |> add "A" 3 |> add "B" 2 |> get
    --> 5

-}
add : String -> Int -> GCounter -> GCounter
add id n counter =
    Dict.update id
        (\mv ->
            case mv of
                Nothing ->
                    Just (max 0 n)

                Just v ->
                    Just (v + max 0 n)
        )
        counter


{-| merge a remote counter into the local counter

    let
        local = init |> add "A" 4
        remote = init |> add "B" 3
        newLocal = merge remote local
    in
        get newLocal --> 7

-}
merge : GCounter -> GCounter -> GCounter
merge otherCounter myCounter =
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
        otherCounter
        myCounter
        Dict.empty


get : GCounter -> Int
get counter =
    Dict.foldl (\_ value acc -> acc + value) 0 counter
