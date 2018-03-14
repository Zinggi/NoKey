module Helper exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Set exposing (Set)
import Char
import Random.Pcg.Extended as RandomE exposing (Generator)
import Random.Pcg as RandomP
import Random.Pcg.Interop as RandomP
import Random
import BigInt exposing (BigInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Uuid
import Time exposing (Time)
import Task exposing (Task)


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


mapModel : (a -> b) -> ( a, c ) -> ( b, c )
mapModel fn ( a, c ) =
    ( fn a, c )


andThenUpdate : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThenUpdate fn ( m, cmd1 ) =
    let
        ( newM, cmd2 ) =
            fn m
    in
        ( newM, Cmd.batch [ cmd1, cmd2 ] )


andThenUpdateIf : Bool -> (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThenUpdateIf shouldUpdate =
    if shouldUpdate then
        andThenUpdate
    else
        (\_ m -> m)



-- Task


performWithTimestamp : (Time -> v -> msg) -> Task Never v -> Cmd msg
performWithTimestamp toMsg task =
    Time.now
        |> Task.andThen (\time -> Task.map (toMsg time) task)
        |> Task.perform identity


attemptWithTimestamp : (Time -> Result x a -> msg) -> Task x a -> Cmd msg
attemptWithTimestamp toMsg task =
    Time.now
        |> Task.andThen
            (\time ->
                Task.map (\a -> ( time, a )) task
                    |> Task.mapError (\x -> ( time, x ))
            )
        |> Task.attempt
            (\res ->
                case res of
                    Ok ( t, a ) ->
                        toMsg t (Ok a)

                    Err ( t, x ) ->
                        toMsg t (Err x)
            )



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


insertOrUpdate : comparable -> a -> (a -> a) -> Dict comparable a -> Dict comparable a
insertOrUpdate key insert update dict =
    Dict.update key
        (\mayVal ->
            case mayVal of
                Just val ->
                    Just <| update val

                Nothing ->
                    Just <| insert
        )
        dict


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


{-| Given a list of strings, find the first non equal characters, in groups of 4.
findNonEqualBeginning ["aaaabbbbcccc", "aaaaccccdddd", "eeeeddddffff"] ->
["aaaabbbb", "aaaacccc", "eeeedddd"]
findNonEqualBeginning ["abcd", "aced"] -> ["abcd", "aced"]
-}
findNonEqualBeginning : List String -> List String
findNonEqualBeginning ids =
    case ids of
        [] ->
            []

        [ id ] ->
            [ "" ]

        first :: rest ->
            findNonEqualBeginningHelper (List.length ids) (roundUpToMultiple 4 (String.length first)) ids


findNonEqualBeginningHelper total n ids =
    let
        groups =
            Dict.groupBy (String.left n) ids
    in
        if Dict.size groups >= total then
            findNonEqualBeginningHelper total (n - 4) ids
        else
            List.map (String.left (n + 4)) ids


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



-- Int


roundUpToMultiple : Int -> Int -> Int
roundUpToMultiple multiple num =
    if multiple == 0 then
        num
    else
        let
            remainder =
                abs num % multiple
        in
            if remainder == 0 then
                num
            else if remainder < 0 then
                -(abs num - remainder)
            else
                num + multiple - remainder



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


decodeTuple3 : Decoder a -> Decoder b -> Decoder c -> Decoder ( a, b, c )
decodeTuple3 dA dB dC =
    JD.map3 (,,) (JD.index 0 dA) (JD.index 1 dB) (JD.index 2 dC)


decodeTuple4 : Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder ( a, b, c, d )
decodeTuple4 dA dB dC dD =
    JD.map4 (,,,) (JD.index 0 dA) (JD.index 1 dB) (JD.index 2 dC) (JD.index 3 dD)


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


encodeTuple3 : (a -> Value) -> (b -> Value) -> (c -> Value) -> ( a, b, c ) -> Value
encodeTuple3 fA fB fC ( a, b, c ) =
    JE.list [ fA a, fB b, fC c ]


encodeTuple4 : (a -> Value) -> (b -> Value) -> (c -> Value) -> (d -> Value) -> ( a, b, c, d ) -> Value
encodeTuple4 fA fB fC fD ( a, b, c, d ) =
    JE.list [ fA a, fB b, fC c, fD d ]


encodeSet : (comparable -> Value) -> Set comparable -> Value
encodeSet valueEncoder set =
    JE.list (Set.toList set |> List.map valueEncoder)



-- Random


pcgToCore : RandomP.Seed -> Random.Seed
pcgToCore seedP =
    Random.initialSeed (RandomP.step (RandomP.int RandomP.minInt RandomP.maxInt) seedP |> Tuple.first)


coreToPcg : Random.Seed -> RandomP.Seed
coreToPcg seedC =
    RandomP.initialSeed (Random.step (Random.int Random.minInt Random.maxInt) seedC |> Tuple.first)


randomUUID : RandomP.Generator String
randomUUID =
    RandomP.map Uuid.toString Uuid.uuidGenerator


groupPwGenerator : Generator String
groupPwGenerator =
    -- 0 - 1114111 is all unicode
    -- 0 - 127 is all ascii
    -- 0 - 255 extended ascii
    -- TODO: how to maximize characters? and how big to make it?
    -- Right now definitely too small!!
    RandomE.map String.fromList (RandomE.list 16 (charGenerator 0 255))


charGenerator : Int -> Int -> Generator Char
charGenerator start end =
    RandomE.map Char.fromCode (RandomE.int start end)


flatten : List (Generator a) -> Generator (List a)
flatten gens =
    case gens of
        g :: gs ->
            g
                |> RandomE.andThen
                    (\a ->
                        flatten gs
                            |> RandomE.map (\aS -> a :: aS)
                    )

        [] ->
            RandomE.constant []


{-| Sample a subset of length n of the given list.
-}
sampleSubset : Int -> List a -> Generator (List a)
sampleSubset n set =
    if n <= 0 then
        RandomE.constant []
    else
        RandomE.sample set
            |> RandomE.andThen
                (\maybeElem ->
                    case maybeElem of
                        Nothing ->
                            RandomE.constant []

                        Just e ->
                            sampleSubset (n - 1) (List.filter (\a -> a /= e) set)
                                |> RandomE.map (\l -> e :: l)
                )


bigIntMax : BigInt -> Generator BigInt
bigIntMax m =
    let
        l =
            BigInt.toString m |> String.length
    in
        bigIntDigits l
            |> RandomE.andThen
                (\n ->
                    if BigInt.lt n m then
                        RandomE.constant n
                    else
                        -- if it doesn meet requirements, try again
                        -- this should terminate in max ~10 rounds, e.g. if m = 11111
                        -- then then the chance of producing a 0 as first char is 1/10
                        bigIntMax m
                )


bigIntDigits : Int -> Generator BigInt
bigIntDigits n =
    RandomE.list n (RandomE.int 0 9)
        |> RandomE.map
            (List.map toString
                >> String.join ""
                >> BigInt.fromString
                >> Maybe.withDefault (BigInt.fromInt 0)
            )
