module Crdt.TimestampedVersionRegister
    exposing
        ( TimestampedVersionRegister
        , init
        , set
        , merge
        , get
        , encode
        , update
        , decoder
        , map
        )

import Time exposing (Time)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Crdt.VClock as VClock exposing (VClock, PartialOrder(..))


type alias TimestampedVersionRegister a =
    { version : VClock, timestamp : Time, value : a }


init : String -> Time -> a -> TimestampedVersionRegister a
init id t v =
    { version = VClock.init |> VClock.increment id, value = v, timestamp = t }


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


update : String -> Time -> (a -> a) -> TimestampedVersionRegister a -> TimestampedVersionRegister a
update id t fn reg =
    set id t (fn (get reg)) reg


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


{-| CAUTION: this doesn't update the CRDT and can easily break the CRDT.
Only use this if you know what you are doing
-}
map : (a -> b) -> TimestampedVersionRegister a -> TimestampedVersionRegister b
map f reg =
    { reg | value = f reg.value }
