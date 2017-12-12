module TimestampedVersionRegister exposing (TimestampedVersionRegister, init, set, merge, get)

import Time exposing (Time)
import Crdt.VClock as VClock exposing (VClock, PartialOrder(..))


type alias TimestampedVersionRegister a =
    { version : VClock, timestamp : Time, value : a }


init : Time -> a -> TimestampedVersionRegister a
init t v =
    { version = VClock.init, value = v, timestamp = t }


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
