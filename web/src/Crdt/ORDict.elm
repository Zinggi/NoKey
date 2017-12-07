module Crdt.ORDict exposing (ORDict, init, insert, remove, merge, encode, decoder, get, update)

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Crdt.ORSet as ORSet exposing (ORSet)


type alias ORDict comparable value =
    { keys : ORSet comparable
    , store : Dict comparable value
    }


decoder : Decoder value -> Decoder (ORDict String value)
decoder valueDecoder =
    JD.map2 ORDict
        (JD.field "keys" ORSet.decoder)
        (JD.field "store" (JD.dict valueDecoder))


encode : (value -> Value) -> ORDict String value -> Value
encode encodeValue dict =
    JE.object [ ( "keys", ORSet.encode dict.keys ), ( "store", JE.object (Dict.toList (Dict.map (always encodeValue) dict.store)) ) ]


init : ORDict comparable value
init =
    { keys = ORSet.init, store = Dict.empty }


insert : comparable -> value -> ORDict comparable value -> ORDict comparable value
insert key value dict =
    { keys = ORSet.add key dict.keys, store = Dict.insert key value dict.store }


update : comparable -> (a -> a) -> ORDict comparable a -> ORDict comparable a
update key f dict =
    { dict
        | store =
            Dict.update key
                (\mv ->
                    case mv of
                        Just v ->
                            Just (f v)

                        Nothing ->
                            Nothing
                )
                dict.store
    }


remove : comparable -> ORDict comparable value -> ORDict comparable value
remove key dict =
    { keys = ORSet.remove key dict.keys, store = Dict.remove key dict.store }


get : ORDict comparable value -> Dict comparable value
get dict =
    Set.foldl
        (\key st ->
            case Dict.get key dict.store of
                Just v ->
                    Dict.insert key v st

                Nothing ->
                    Debug.crash "should always have this value"
        )
        Dict.empty
        (ORSet.get dict.keys)


merge : (value -> value -> value) -> ORDict comparable value -> ORDict comparable value -> ORDict comparable value
merge mergeValue dictA dictB =
    let
        newKeys =
            ORSet.merge dictA.keys dictB.keys

        newStore =
            ORSet.get newKeys
                |> Set.foldl
                    (\key st ->
                        case ( Dict.get key dictA.store, Dict.get key dictB.store ) of
                            ( Just va, Just vb ) ->
                                if va == vb then
                                    Dict.insert key va st
                                else
                                    Dict.insert key (mergeValue va vb) st

                            ( Nothing, Just v ) ->
                                Dict.insert key v st

                            ( Just v, Nothing ) ->
                                Dict.insert key v st

                            ( Nothing, Nothing ) ->
                                Debug.crash "This shouldn't happen!"
                    )
                    Dict.empty
    in
        { keys = newKeys, store = newStore }
