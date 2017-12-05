module Crdt.LWWRegister exposing (LWWRegister)

import Crdt.VClock as VClock exposing (VClock, PartialOrder(..))


type alias LWWRegister a =
    { time : VClock, value : a }


init : a -> LWWRegister a
init v =
    { time = VClock.init, value = v }


set : String -> a -> LWWRegister a -> LWWRegister a
set id v reg =
    { time = VClock.increment id reg.time, value = v }


merge : LWWRegister a -> LWWRegister a -> Result ( a, a, VClock ) (LWWRegister a)
merge a b =
    case VClock.compare a.time b.time of
        After ->
            Ok a

        Before ->
            Ok b

        equalOrConcurrent ->
            if a.value == b.value then
                Ok { time = VClock.merge a.time b.time, value = a.value }
            else
                Err ( a.value, b.value, VClock.merge a.time b.time )


mergeBiggerWins : LWWRegister comparable -> LWWRegister comparable -> LWWRegister comparable
mergeBiggerWins a b =
    case merge a b of
        Ok r ->
            r

        Err ( av, bv, c ) ->
            if av > bv then
                { a | time = c }
            else
                { b | time = c }
