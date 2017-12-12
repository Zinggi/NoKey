module Crdt.GSet exposing (GSet, add, merge, init, get, decoder, customDecoder, encode, encodeCustom)

import Set exposing (Set)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Helper exposing (encodeSet)


type alias GSet comparable =
    Set comparable


init : GSet comparable
init =
    Set.empty


encode : GSet String -> Value
encode =
    encodeSet JE.string


encodeCustom : (comparable -> Value) -> GSet comparable -> Value
encodeCustom =
    encodeSet


decoder : Decoder (GSet String)
decoder =
    JD.set JD.string


customDecoder : Decoder comparable -> Decoder (GSet comparable)
customDecoder =
    JD.set


get : GSet comparable -> Set comparable
get =
    identity


add : comparable -> GSet comparable -> GSet comparable
add =
    Set.insert


merge : GSet comparable -> GSet comparable -> GSet comparable
merge =
    Set.union
