module Crdt.TimestampedVersionRegister exposing (TimestampedVersionRegister, init, set, merge, get, encode, decoder)

import Time exposing (Time)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Crdt.VClock as VClock exposing (VClock, PartialOrder(..))


type alias TimestampedVersionRegister a =
    { version : VClock, timestamp : Time, value : a }


init : Time -> a -> TimestampedVersionRegister a
init t v =
    { version = VClock.init, value = v, timestamp = t }


decoder : Decoder a -> Decoder (TimestampedVersionRegister a)
decoder valueDecoder =
    JD.map3 TimestampedVersionRegister
        (JD.field "version" VClock.decoder)
        (JD.field "timestamp" JD.float)
        (JD.field "value" valueDecoder)


encode : (a -> Value) -> TimestampedVersionRegister a -> Value
encode valueEncoder reg =
    JE.object
        [ ( "version", VClock.encode reg.version )
        , ( "timestamp", JE.float reg.timestamp )
        , ( "value", valueEncoder reg.value )
        ]


set : String -> Time -> a -> TimestampedVersionRegister a -> TimestampedVersionRegister a
set id t v reg =
    { version = VClock.increment id reg.version, value = v, timestamp = t }


{-| The merge function ignores the timestamp, if the version number can determin a clear winner.
Else, it uses a timestamp as a last resort
-}
merge : TimestampedVersionRegister a -> TimestampedVersionRegister a -> TimestampedVersionRegister a
merge a b =
    case VClock.compare a.version b.version of
        After ->
            a

        Before ->
            b

        equalOrConcurrent ->
            if a.timestamp > b.timestamp then
                { version = VClock.merge a.version b.version, value = a.value, timestamp = a.timestamp }
            else
                { version = VClock.merge a.version b.version, value = b.value, timestamp = b.timestamp }


get : TimestampedVersionRegister a -> a
get { value } =
    value
