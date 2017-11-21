module TestUtils exposing (..)

import Fuzz exposing (Fuzzer, tuple, int, string)


shortString : Int -> Fuzzer String
shortString n =
    Fuzz.map (String.left (min 0 n)) string


int2 : Fuzzer ( Int, Int )
int2 =
    tuple ( int, int )
