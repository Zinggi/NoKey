module Helper exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Random.Pcg.Extended as Random exposing (Generator)
import Random.Pcg as RandomP
import BigInt exposing (BigInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Uuid
import Time exposing (Time)
import Task


-- Update


noCmd : a -> ( a, Cmd msg )
noCmd a =
    ( a, Cmd.none )


withCmds : List (Cmd msg) -> a -> ( a, Cmd msg )
withCmds cmds a =
    ( a, Cmd.batch cmds )


addCmds : List (Cmd msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmds cmds ( a, cmd ) =
    ( a, Cmd.batch (cmd :: cmds) )


withTimestamp : (Time -> msg) -> Cmd msg
withTimestamp toMsg =
    Time.now
        |> Task.perform (toMsg)



-- Maybe


{-| Use this when you know for certain that a Maybe will be Just a.
If you were wrong, it will crash!
-}
crashOnNothing : String -> Maybe a -> a
crashOnNothing s ma =
    case ma of
        Just a ->
            a

        Nothing ->
            Debug.crash s


boolToMaybe : Bool -> a -> Maybe a
boolToMaybe b a =
    if b then
        Just a
    else
        Nothing



-- Result


{-| Combine a list of results into a single result (holding a list).
-}
combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])



-- Dict


filterDict : Dict comparable ( Bool, a ) -> List a
filterDict sets =
    Dict.toList sets
        |> List.filterMap
            (\( k, ( b, s ) ) ->
                if b then
                    Just s
                else
                    Nothing
            )


removeAllExcept : comparable -> Dict comparable value -> Dict comparable value
removeAllExcept key dict =
    case Dict.get key dict of
        Just v ->
            Dict.singleton key v

        Nothing ->
            Dict.empty



-- String


replaceIndices : List ( Int, Char ) -> String -> String
replaceIndices indices s =
    List.foldl (\( i, c ) str -> replaceCharAtIndex i c str) s indices


replaceCharAtIndex : Int -> Char -> String -> String
replaceCharAtIndex i c s =
    indexedMap
        (\ii cc ->
            if i == ii then
                c
            else
                cc
        )
        s


indexedMap : (Int -> Char -> Char) -> String -> String
indexedMap f s =
    String.toList s
        |> List.indexedMap f
        |> String.fromList



-- List


maybeToList : Maybe a -> List a
maybeToList mayA =
    case mayA of
        Just a ->
            [ a ]

        Nothing ->
            []


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



-- Bool


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1
    else
        0



-- Decoder


decodeTuple : Decoder a -> Decoder ( a, a )
decodeTuple valueDecoder =
    JD.map2 (,) (JD.index 0 valueDecoder) (JD.index 1 valueDecoder)


decodeTuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
decodeTuple2 valueDecoderA valueDecoderB =
    JD.map2 (,) (JD.index 0 valueDecoderA) (JD.index 1 valueDecoderB)


decodeSet : Decoder comparable -> Decoder (Set comparable)
decodeSet valueDecoder =
    JD.map Set.fromList (JD.list valueDecoder)



-- Encoder


encodeTuple : (a -> Value) -> ( a, a ) -> Value
encodeTuple valueEncoder ( a, b ) =
    JE.list [ valueEncoder a, valueEncoder b ]


encodeTuple2 : (a -> Value) -> (b -> Value) -> ( a, b ) -> Value
encodeTuple2 valueEncoderA valueEncoderB ( a, b ) =
    JE.list [ valueEncoderA a, valueEncoderB b ]


encodeSet : (comparable -> Value) -> Set comparable -> Value
encodeSet valueEncoder set =
    JE.list (Set.toList set |> List.map valueEncoder)



-- Random


randomUUID : RandomP.Generator String
randomUUID =
    RandomP.map Uuid.toString Uuid.uuidGenerator


flatten : List (Generator a) -> Generator (List a)
flatten gens =
    case gens of
        g :: gs ->
            g
                |> Random.andThen
                    (\a ->
                        flatten gs
                            |> Random.map (\aS -> a :: aS)
                    )

        [] ->
            Random.constant []


{-| Sample a subset of length n of the given list.
-}
sampleSubset : Int -> List a -> Generator (List a)
sampleSubset n set =
    if n <= 0 then
        Random.constant []
    else
        Random.sample set
            |> Random.andThen
                (\maybeElem ->
                    case maybeElem of
                        Nothing ->
                            Random.constant []

                        Just e ->
                            sampleSubset (n - 1) (List.filter (\a -> a /= e) set)
                                |> Random.map (\l -> e :: l)
                )


bigIntMax : BigInt -> Generator BigInt
bigIntMax m =
    let
        l =
            BigInt.toString m |> String.length
    in
        bigIntDigits l
            |> Random.andThen
                (\n ->
                    if BigInt.lt n m then
                        Random.constant n
                    else
                        -- if it doesn meet requirements, try again
                        -- this should terminate in max ~10 rounds, e.g. if m = 11111
                        -- then then the chance of producing a 0 as first char is 1/10
                        bigIntMax m
                )


bigIntDigits : Int -> Generator BigInt
bigIntDigits n =
    Random.list n (Random.int 0 9)
        |> Random.map
            (List.map toString
                >> String.join ""
                >> BigInt.fromString
                >> Maybe.withDefault (BigInt.fromInt 0)
            )
