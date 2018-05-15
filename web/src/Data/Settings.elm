module Data.Settings exposing (Settings, SharedSettings, encode, decoder, init, setAllowLevel1, minSecurityLevel, merge, get, set)

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD exposing (required, optional)
import Time exposing (Time)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)


{- TODO: update news + sync if read tutorial

   save last seen update news for version number:
       start at v 0.0.0.
       If the current version number is bigger than our last seen,
       show all news with v >= currentV

       save news in a Dict Version Content

       type alias UpdateNews msg = Dict Version (Content msg)

       type alias Content msg = { title : String , shortSummery : Element msg, details : Element msg }

       save read news and sync, so we only have to read once

-}


type alias SharedSettings =
    { allowLevel1 : Maybe (TimestampedVersionRegister Bool)
    , timeUntilAutoLock : Maybe (TimestampedVersionRegister Time)
    }


type alias Settings =
    { allowLevel1 : Bool
    , timeUntilAutoLock : Time
    }


get : SharedSettings -> Settings
get shared =
    { allowLevel1 = getMaybe .allowLevel1 .allowLevel1 shared
    , timeUntilAutoLock = getMaybe .timeUntilAutoLock .timeUntilAutoLock shared
    }


set : String -> Time -> Settings -> SharedSettings -> SharedSettings
set id time settings shared =
    { allowLevel1 = setMaybe id time .allowLevel1 .allowLevel1 settings shared
    , timeUntilAutoLock = setMaybe id time .timeUntilAutoLock .timeUntilAutoLock settings shared
    }


setMaybe id time f1 f2 new oldShared =
    let
        old =
            get oldShared
    in
        if f1 old /= f1 new then
            case f2 oldShared of
                Just v ->
                    Just <| TimestampedVersionRegister.set id time (f1 new) v

                Nothing ->
                    Just <| TimestampedVersionRegister.init id time (f1 new)
        else
            f2 oldShared


getMaybe f1 f2 options =
    Maybe.map TimestampedVersionRegister.get (f1 options) |> Maybe.withDefault (f2 defaults)


setAllowLevel1 : Bool -> Settings -> Settings
setAllowLevel1 b o =
    { o | allowLevel1 = b }


minSecurityLevel : SharedSettings -> Int
minSecurityLevel options =
    if (get options).allowLevel1 then
        1
    else
        2


init : SharedSettings
init =
    { allowLevel1 = Nothing
    , timeUntilAutoLock = Nothing
    }


mergeMaybe m1 m2 =
    case ( m1, m2 ) of
        ( Just v1, Just v2 ) ->
            Just (TimestampedVersionRegister.merge v1 v2)

        ( Just v1, Nothing ) ->
            Just v1

        ( Nothing, Just v2 ) ->
            Just v2

        ( Nothing, Nothing ) ->
            Nothing


merge : SharedSettings -> SharedSettings -> SharedSettings
merge s1 s2 =
    { allowLevel1 = mergeMaybe s1.allowLevel1 s2.allowLevel1
    , timeUntilAutoLock = mergeMaybe s1.timeUntilAutoLock s2.timeUntilAutoLock
    }


defaults : Settings
defaults =
    { allowLevel1 = False, timeUntilAutoLock = 10 * Time.minute }


maybeEncode : String -> (a -> Value) -> Maybe a -> List ( String, Value )
maybeEncode tag f m =
    case m of
        Just v ->
            [ ( tag, f v ) ]

        Nothing ->
            []


encode : SharedSettings -> Value
encode opt =
    JE.object <|
        List.concat
            [ maybeEncode "allowLevel1" (TimestampedVersionRegister.encode JE.bool) opt.allowLevel1
            , maybeEncode "timeUntilAutoLock" (TimestampedVersionRegister.encode JE.float) opt.timeUntilAutoLock
            ]


decoder : Decoder SharedSettings
decoder =
    JD.decode (\al1 t -> { allowLevel1 = al1, timeUntilAutoLock = t })
        |> optional "allowLevel1" (TimestampedVersionRegister.decoder JD.bool |> JD.map Just) Nothing
        |> optional "timeUntilAutoLock" (TimestampedVersionRegister.decoder JD.float |> JD.map Just) Nothing
