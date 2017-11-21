module FiniteField exposing (..)

import BigInt exposing (BigInt)


type alias Field =
    { add : NxNtoN, mul : NxNtoN, pow : NxNtoN }


type alias NxNtoN =
    BigInt -> BigInt -> BigInt


type alias Prime =
    BigInt


bigPrime =
    BigInt.fromString "78964309289234503966245545309514784011238902738532633290445390498470508979557"
        |> Maybe.withDefault (BigInt.fromInt 31)


primeBiggerThan : BigInt -> Prime
primeBiggerThan n =
    -- TODO: don't hardcode the prime
    bigPrime


makeField : Prime -> Field
makeField p =
    { add = \a b -> BigInt.mod (BigInt.add a b) p
    , mul = \a b -> BigInt.mod (BigInt.mul a b) p
    , pow = \b e -> bigIntPowMod b e p
    }


bigIntPowMod b e p =
    bigIntPowModHelp b e p (BigInt.fromInt 1)


bigIntPowModHelp b e p acc =
    if BigInt.lte e (BigInt.fromInt 0) then
        BigInt.fromInt 1
    else
        bigIntPowModHelp b (BigInt.sub e (BigInt.fromInt -1)) p (BigInt.mod (BigInt.mul b acc) p)
