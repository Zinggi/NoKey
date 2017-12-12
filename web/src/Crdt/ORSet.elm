module Crdt.ORSet exposing (ORSet, init, add, remove, get, merge, decoder, encode, encodeCustom, equal, reset, customDecoder)

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
import Helper exposing (decodeTuple, encodeTuple, encodeSet, decodeSet)


type alias ORSet comparable =
    { seed : Seed
    , data : Dict comparable ( Set Int, Set Int )
    }


reset : ORSet comparable -> ORSet comparable
reset set =
    { set | data = Dict.empty }


equal : ORSet comparable -> ORSet comparable -> Bool
equal a b =
    a.data == b.data


decoder : Decoder (ORSet String)
decoder =
    JD.map (\d -> { data = d, seed = Random.initialSeed 0 })
        (JD.dict (decodeTuple (decodeSet JD.int)))


customDecoder : Decoder comparable -> Decoder (ORSet comparable)
customDecoder keyDecoder =
    JD.map (\d -> { data = d, seed = Random.initialSeed 0 })
        (JD.dict2 keyDecoder (decodeTuple (decodeSet JD.int)))


{-| This encoder won't encode the seed. Other clients don't need to know our internal seed.
-}
encode : ORSet String -> Value
encode =
    encodeCustom identity


encodeCustom : (comparable -> String) -> ORSet comparable -> Value
encodeCustom keyTrans set =
    JE.dict keyTrans (encodeTuple (encodeSet JE.int)) set.data


init : Seed -> ORSet comparable
init seed =
    { seed = seed, data = Dict.empty }


add : comparable -> ORSet comparable -> ORSet comparable
add e set =
    let
        ( randInt, newSeed ) =
            Random.step (Random.int Random.minInt Random.maxInt) set.seed

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
