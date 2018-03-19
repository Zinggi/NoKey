module Crdt.SingleVersionRegister exposing (SingleVersionRegister, init, update, merge, decoder, encode, get, set, map)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Helper exposing (decodeTuple2, encodeTuple2)


type alias SingleVersionRegister a =
    ( Int, a )


init : a -> SingleVersionRegister a
init a =
    ( 0, a )


get : SingleVersionRegister a -> a
get ( _, a ) =
    a


set : a -> SingleVersionRegister a -> SingleVersionRegister a
set newValue ( n, _ ) =
    ( n + 1, newValue )


update : (a -> a) -> SingleVersionRegister a -> SingleVersionRegister a
update f ( n, v ) =
    ( n + 1, f v )


decoder : Decoder a -> Decoder (SingleVersionRegister a)
decoder valueDecoder =
    decodeTuple2 JD.int valueDecoder


encode : (a -> Value) -> SingleVersionRegister a -> Value
encode encodeValue =
    encodeTuple2 JE.int encodeValue


merge : SingleVersionRegister a -> SingleVersionRegister a -> SingleVersionRegister a
merge ( nOther, a ) ( nMy, b ) =
    if nOther >= nMy then
        ( nOther, a )
    else
        ( nMy, b )


{-| CAUTION: doesn't increment version, only use if you know what you are doing
-}
map : (a -> b) -> SingleVersionRegister a -> SingleVersionRegister b
map f ( n, v ) =
    ( n, f v )
