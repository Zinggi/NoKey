module Crdt.ORDict
    exposing
        ( ORDict
        , init
        , insert
        , remove
        , merge
        , encode
        , encode2
        , decoder
        , keys
        , get
        , update
        , equal
        , reset
        , decoder2
        , fromDict
        , getWith
        , updateWithDict
        , updateIf
        , encodeComplete
        , completeDecoder
        , completeDecoder2
        , updateOrInsert
        , map
        , resetExceptOne
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Random.Pcg exposing (Seed)
import Crdt.ORSet as ORSet exposing (ORSet)
import Helper exposing (removeAllExcept)


type alias ORDict comparable value =
    { keys : ORSet comparable
    , store : Dict comparable value
    }


resetExceptOne : comparable -> ORDict comparable value -> ORDict comparable value
resetExceptOne key dict =
    { keys =
        ORSet.resetExceptOne key dict.keys
            |> ORSet.remove key
            |> ORSet.add key
    , store = removeAllExcept key dict.store
    }


keys : ORDict comparable v -> Set comparable
keys dict =
    ORSet.get dict.keys


reset : ORDict comparable value -> ORDict comparable value
reset dict =
    { keys = ORSet.reset dict.keys, store = Dict.empty }


equal : ORDict comparable value -> ORDict comparable value -> Bool
equal a b =
    a.store == b.store && ORSet.equal a.keys b.keys


decoder : Decoder value -> Seed -> Decoder (ORDict String value)
decoder valueDecoder seed =
    JD.map2 ORDict
        (JD.field "keys" (ORSet.decoder seed))
        (JD.field "store" (JD.dict valueDecoder))


completeDecoder : Decoder value -> Decoder (ORDict String value)
completeDecoder valueDecoder =
    JD.map2 ORDict
        (JD.field "keys" ORSet.completeDecoder)
        (JD.field "store" (JD.dict valueDecoder))


decoder2 : Decoder comparable -> Decoder value -> Seed -> Decoder (ORDict comparable value)
decoder2 keyDecoder valueDecoder seed =
    JD.map2 ORDict
        (JD.field "keys" (ORSet.customDecoder keyDecoder seed))
        (JD.field "store" (JD.dict2 keyDecoder valueDecoder))


completeDecoder2 : Decoder comparable -> Decoder value -> Decoder (ORDict comparable value)
completeDecoder2 keyDecoder valueDecoder =
    JD.map2 ORDict
        (JD.field "keys" (ORSet.completeDecoder2 keyDecoder))
        (JD.field "store" (JD.dict2 keyDecoder valueDecoder))


encode : (value -> Value) -> ORDict String value -> Value
encode encodeValue dict =
    JE.object [ ( "keys", ORSet.encode dict.keys ), ( "store", JE.object (Dict.toList (Dict.map (always encodeValue) dict.store)) ) ]


encode2 : (comparable -> Value) -> (value -> Value) -> ORDict comparable value -> Value
encode2 encodeKey encodeValue dict =
    JE.object [ ( "keys", ORSet.encodeCustom encodeKey dict.keys ), ( "store", JE.dict (encodeKey >> JE.encode 0) encodeValue dict.store ) ]


encodeComplete : (comparable -> String) -> (value -> Value) -> ORDict comparable value -> Value
encodeComplete encodeKey encodeValue dict =
    JE.object [ ( "keys", ORSet.encodeComplete encodeKey dict.keys ), ( "store", JE.dict encodeKey encodeValue dict.store ) ]


init : Seed -> ORDict comparable value
init seed =
    { keys = ORSet.init seed, store = Dict.empty }


fromDict : Seed -> Dict comparable value -> ORDict comparable value
fromDict seed =
    Dict.foldl insert (init seed)


insert : comparable -> value -> ORDict comparable value -> ORDict comparable value
insert key value dict =
    { keys = ORSet.add key dict.keys, store = Dict.insert key value dict.store }


updateIf : (comparable -> a -> Bool) -> (comparable -> a -> a) -> ORDict comparable a -> ORDict comparable a
updateIf ifFn updateFn dict =
    { dict
        | store =
            Dict.map
                (\key val ->
                    if ifFn key val then
                        updateFn key val
                    else
                        val
                )
                dict.store
    }


updateOrInsert : comparable -> (a -> a) -> a -> ORDict comparable a -> ORDict comparable a
updateOrInsert key f value dict =
    let
        newStore =
            Dict.update key
                (\mv ->
                    case mv of
                        Just v ->
                            Just (f v)

                        Nothing ->
                            Just value
                )
                dict.store
    in
        if ORSet.member key dict.keys then
            { dict | store = newStore }
        else
            { keys = ORSet.add key dict.keys, store = newStore }


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


updateWithDict : (a -> b -> b) -> Dict comparable a -> ORDict comparable b -> ORDict comparable b
updateWithDict f dict ordict =
    Dict.foldl
        (\key value acc ->
            update key (f value) acc
        )
        ordict
        dict


{-| CAUTION: using this can very easily break the CRDT guarantees.
Only use when you know what you are doing
-}
map : (comparable -> a -> b) -> ORDict comparable a -> ORDict comparable b
map f dict =
    { dict | store = Dict.map f dict.store }


remove : comparable -> ORDict comparable value -> ORDict comparable value
remove key dict =
    { keys = ORSet.remove key dict.keys, store = Dict.remove key dict.store }


getWith : (a -> b) -> ORDict comparable a -> Dict comparable b
getWith f dict =
    Set.foldl
        (\key st ->
            case Dict.get key dict.store of
                Just v ->
                    Dict.insert key (f v) st

                Nothing ->
                    Debug.crash "should always have this value"
        )
        Dict.empty
        (ORSet.get dict.keys)


get : ORDict comparable value -> Dict comparable value
get =
    getWith identity



-- Set.foldl
--     (\key st ->
--         case Dict.get key dict.store of
--             Just v ->
--                 Dict.insert key v st
--             Nothing ->
--                 Debug.crash "should always have this value"
--     )
--     Dict.empty
--     (ORSet.get dict.keys)


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA
-}
merge : (value -> value -> value) -> ORDict comparable value -> ORDict comparable value -> ORDict comparable value
merge mergeValue otherDict myDict =
    let
        newKeys =
            ORSet.merge otherDict.keys myDict.keys

        newStore =
            ORSet.get newKeys
                |> Set.foldl
                    (\key st ->
                        case ( Dict.get key myDict.store, Dict.get key otherDict.store ) of
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
