module FiniteField exposing (Field, Prime, makeField, primeBiggerThan, lagrangeInterpolation, getPolynomialPoints, secretPolynom, evalPolynom)

import BigInt exposing (BigInt)
import Random.Pcg as Random exposing (Generator)


--

import Helper exposing (bigIntMax, crashOnNothing)


type alias Field =
    { add : NxNtoN, mul : NxNtoN, pow : NxNtoN, sub : NxNtoN, modInverse : NtoN }


type alias NxNtoN =
    BigInt -> BigInt -> BigInt


type alias NtoN =
    BigInt -> BigInt


type alias Prime =
    BigInt


bigPrime =
    -- TODO: this is only good for up to 32 characters!
    BigInt.fromString "78964309289234503966245545309514784011238902738532633290445390498470508979557"
        |> crashOnNothing "Failed to parse something that is clearly a number..."


primeBiggerThan : BigInt -> Prime
primeBiggerThan n =
    -- TODO: don't hardcode the prime
    bigPrime


makeField : Prime -> Field
makeField p =
    { add = \a b -> BigInt.mod (BigInt.add a b) p
    , mul = \a b -> BigInt.mod (BigInt.mul a b) p
    , pow = \b e -> bigIntPowMod b e p
    , sub = \a b -> BigInt.mod (BigInt.sub (BigInt.add a p) b) p
    , modInverse = modInverse p
    }


bigIntPowMod b e p =
    bigIntPowModHelp b e p (BigInt.fromInt 1)


bigIntPowModHelp b e p acc =
    if BigInt.gt e zero then
        let
            acc_ =
                if BigInt.mod e (BigInt.fromInt 2) /= zero then
                    BigInt.mod (BigInt.mul acc b) p
                else
                    acc

            b_ =
                BigInt.mod (BigInt.mul b b) p

            e_ =
                BigInt.div e (BigInt.fromInt 2)
        in
            bigIntPowModHelp b_ e_ p acc_
    else
        acc


one : BigInt
one =
    BigInt.fromInt 1


zero : BigInt
zero =
    BigInt.fromInt 0



--------------------------------------------------------------------------------
-- The functions here were heavyly inspired from this python implementation:
-- https://github.com/blockstack/secret-sharing/blob/master/secretsharing/polynomials.py
--------------------------------------------------------------------------------


egcd : BigInt -> BigInt -> ( BigInt, BigInt, BigInt )
egcd a b =
    if a == zero then
        ( b, zero, one )
    else
        let
            ( g, y, x ) =
                egcd (BigInt.mod b a) a
        in
            ( g, BigInt.sub x (BigInt.mul (BigInt.div b a) y), y )


modInverse : BigInt -> BigInt -> BigInt
modInverse p k =
    let
        k_ =
            BigInt.mod k p

        ( _, _, r ) =
            if BigInt.lt k zero then
                egcd p (BigInt.negate k_)
            else
                egcd p k_
    in
        BigInt.mod (BigInt.add p r) p


getPolynomialPoints : Field -> List BigInt -> Int -> List ( Int, BigInt )
getPolynomialPoints f coeffs numPoints =
    List.range 1 numPoints
        |> List.map (\x -> ( x, evalPolynom f coeffs (BigInt.fromInt x) ))


evalPolynom : Field -> List BigInt -> BigInt -> BigInt
evalPolynom f coeffs x =
    Tuple.first <|
        List.foldl
            (\ci ( y_acc, i ) ->
                let
                    term =
                        f.mul ci (f.pow x i)
                in
                    ( f.add y_acc term, BigInt.add i one )
            )
            ( zero, zero )
            coeffs


lagrangeInterpolation : Field -> List ( BigInt, BigInt ) -> BigInt -> BigInt
lagrangeInterpolation f points x =
    let
        -- lagrange basis polynomial l_i(x)
        l_i_x =
            getLagrangePolynomial f points x
    in
        List.foldl (\( xi, yi ) acc -> f.add acc (f.mul yi (l_i_x xi))) zero points


getLagrangePolynomial : Field -> List ( BigInt, BigInt ) -> BigInt -> BigInt -> BigInt
getLagrangePolynomial f points x xi =
    let
        ( a, b ) =
            List.foldl
                (\( xj, yj ) ( num, denom ) ->
                    if xi == xj then
                        ( num, denom )
                    else
                        ( f.mul num (f.sub x xj), f.mul denom (f.sub xi xj) )
                )
                ( BigInt.fromInt 1, BigInt.fromInt 1 )
                points
    in
        f.mul (f.modInverse b) a


secretPolynom : Prime -> BigInt -> Int -> Generator (List BigInt)
secretPolynom p c0 numOfCoeffs =
    Random.list numOfCoeffs (bigIntMax p)
        |> Random.map (\x -> c0 :: x)
