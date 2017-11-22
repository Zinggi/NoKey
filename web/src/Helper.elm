module Helper exposing (..)

import Dict exposing (Dict)
import Random.Pcg as Random exposing (Generator)
import BigInt exposing (BigInt)


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



-- Random


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
