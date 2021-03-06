module Crdt.ORSet
    exposing
        ( ORSet
        , init
        , set
        , add
        , remove
        , get
        , member
        , merge
        , decoder
        , encode
        , encodeCustom
        , equal
        , reset
        , customDecoder
        , encodeComplete
        , completeDecoder
        , completeDecoder2
        , resetExceptOne
        )

{-| This implements an Observed Remove Set, Conflict-free Replicated Data Structure (CRDT)

**Causion**
since this data structure has some internal state for randomness, you have to use `equal` instead of `==`

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Random.Pcg as Random exposing (Seed)
import Helper exposing (decodeTuple, encodeTuple, encodeSet, decodeSet, removeAllExcept)


type alias ORSet comparable =
    { seed : Seed
    , data : Dict comparable ( Set Int, Set Int )
    }


reset : ORSet comparable -> ORSet comparable
reset set =
    { set | data = Dict.empty }


getUniqueId : ORSet comparable -> ( Int, Seed )
getUniqueId set =
    let
        ( n, seed ) =
            Random.step (Random.int Random.minInt Random.maxInt) set.seed

        ids =
            set.data
                |> Dict.values
                |> List.foldl
                    (\( adds, removes ) acc ->
                        Set.union (Set.union adds removes) acc
                    )
                    Set.empty
    in
        if Set.member n ids then
            -- try again
            getUniqueId { set | seed = seed }
        else
            ( n, seed )


resetExceptOne : comparable -> ORSet comparable -> ORSet comparable
resetExceptOne key set =
    { set | data = removeAllExcept key set.data }


equal : ORSet comparable -> ORSet comparable -> Bool
equal a b =
    a.data == b.data


decoder : Seed -> Decoder (ORSet String)
decoder seed =
    JD.map (\d -> { data = d, seed = seed })
        (JD.dict (decodeTuple (decodeSet JD.int)))


customDecoder : Decoder comparable -> Seed -> Decoder (ORSet comparable)
customDecoder keyDecoder seed =
    JD.map (\d -> { data = d, seed = seed })
        (JD.dict2 keyDecoder (decodeTuple (decodeSet JD.int)))


completeDecoder : Decoder (ORSet String)
completeDecoder =
    JD.map2 (\d s -> { data = d, seed = s })
        (JD.field "data" (JD.dict (decodeTuple (decodeSet JD.int))))
        (JD.field "seed" Random.fromJson)


completeDecoder2 : Decoder comparable -> Decoder (ORSet comparable)
completeDecoder2 keyDecoder =
    JD.map2 (\d s -> { data = d, seed = s })
        (JD.field "data" (JD.dict2 keyDecoder (decodeTuple (decodeSet JD.int))))
        (JD.field "seed" Random.fromJson)


{-| This encoder won't encode the seed. Other clients don't need to know our internal seed.
-}
encode : ORSet String -> Value
encode set =
    JE.dict identity (encodeTuple (encodeSet JE.int)) set.data


encodeCustom : (comparable -> Value) -> ORSet comparable -> Value
encodeCustom keyTrans set =
    JE.dict (\s -> keyTrans s |> JE.encode 0) (encodeTuple (encodeSet JE.int)) set.data


{-| Encode with the seed
-}
encodeComplete : (comparable -> String) -> ORSet comparable -> Value
encodeComplete keyTrans set =
    JE.object
        [ ( "data", JE.dict keyTrans (encodeTuple (encodeSet JE.int)) set.data )
        , ( "seed", Random.toJson set.seed )
        ]


init : Seed -> ORSet comparable
init seed =
    { seed = seed, data = Dict.empty }


{-|

    import Set
    import Random.Pcg as Random

    let
        a = init (Random.initialSeed 0) |> add 1 |> add 42
    in
        get <| set (Set.fromList [42, 35]) a
    --> Set.fromList [35, 42]
-}
set : Set comparable -> ORSet comparable -> ORSet comparable
set set orSet =
    let
        toAdd =
            Set.diff set (get orSet)

        toRemove =
            Set.diff (get orSet) set
    in
        Set.foldl add
            (Set.foldl remove orSet toRemove)
            toAdd


add : comparable -> ORSet comparable -> ORSet comparable
add e set =
    let
        ( randInt, newSeed ) =
            getUniqueId set

        newData =
            Dict.update e
                (\mv ->
                    case mv of
                        Just ( a, r ) ->
                            Just ( Set.insert randInt a, r )

                        Nothing ->
                            Just ( Set.singleton randInt, Set.empty )
                )
                set.data
    in
        { data = newData, seed = newSeed }


remove : comparable -> ORSet comparable -> ORSet comparable
remove e set =
    let
        newData =
            Dict.update e
                (\mv ->
                    case mv of
                        Just ( adds, removes ) ->
                            Just ( adds, Set.union adds removes )

                        Nothing ->
                            Nothing
                )
                set.data
    in
        { set | data = newData }


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA

    import Set
    import Random.Pcg as Random

    let
        a = init (Random.initialSeed 0) |> add 1 |> add 42
        b = init (Random.initialSeed 1) |> add 42 |> remove 42 |> add 2
    in
        merge b a |> get
    --> Set.fromList [1, 2, 42]

-}
merge : ORSet comparable -> ORSet comparable -> ORSet comparable
merge otherSet mySet =
    let
        newData =
            Dict.merge
                (\key a result ->
                    Dict.insert key a result
                )
                (\key ( aa, ar ) ( ba, br ) result ->
                    Dict.insert key ( Set.union aa ba, Set.union ar br ) result
                )
                (\key b result ->
                    Dict.insert key b result
                )
                otherSet.data
                mySet.data
                Dict.empty
    in
        { mySet | data = newData }


{-|

    import Set
    import Random.Pcg as Random

    init (Random.initialSeed 1) |> add 1 |> add 2 |> remove 2 |> get
    --> Set.fromList [1]
-}
get : ORSet comparable -> Set comparable
get set =
    Dict.foldl
        (\key ( add, remove ) acc ->
            if Set.isEmpty (Set.diff add remove) then
                acc
            else
                Set.insert key acc
        )
        Set.empty
        set.data


member : comparable -> ORSet comparable -> Bool
member key set =
    Set.member key (get set)
