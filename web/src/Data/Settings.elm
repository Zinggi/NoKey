module Data.Settings
    exposing
        ( Settings
        , SharedSettings
        , encode
        , decoder
        , init
        , currentVersion
        , allVersions
        , setAllowLevel1
        , markSeen
        , notSeenNews
        , minSecurityLevel
        , merge
        , get
        , set
        , setDoneWithTutorial
        )

import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline as JD exposing (required, optional)
import Set exposing (Set)
import Dict exposing (Dict)
import Time exposing (Time)
import Random.Pcg as Random exposing (Seed)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import Crdt.ORSet as ORSet exposing (ORSet)


{- TODO: update news + sync if read tutorial

   save last seen update news for version number:
       start at v 0.0.0.
       If the current version number is bigger than our last seen,
       show all news with v >= currentV

       save news in a Dict Version Content

       save read news and sync, so we only have to read once

-}


type alias SharedSettings =
    { allowLevel1 : Maybe (TimestampedVersionRegister Bool)
    , timeUntilAutoLock : Maybe (TimestampedVersionRegister Time)
    , hasSeenTutorial : Bool
    , seenNews : ORSet String
    }


type alias Settings =
    { allowLevel1 : Bool
    , timeUntilAutoLock : Time
    , hasSeenTutorial : Bool
    , seenNews : Set String
    }


currentVersion =
    -- TODO!: change if a new version is released
    "0.3.0"


allVersions =
    -- TODO!: update this on big releases, e.g. if you want to include some text
    Set.fromList [ "0.3" ]


get : SharedSettings -> Settings
get shared =
    { allowLevel1 = getMaybe .allowLevel1 .allowLevel1 shared
    , timeUntilAutoLock = getMaybe .timeUntilAutoLock .timeUntilAutoLock shared
    , hasSeenTutorial = shared.hasSeenTutorial
    , seenNews = ORSet.get shared.seenNews
    }


set : String -> Time -> Settings -> SharedSettings -> SharedSettings
set id time settings shared =
    { allowLevel1 = setMaybe id time .allowLevel1 .allowLevel1 settings shared
    , timeUntilAutoLock = setMaybe id time .timeUntilAutoLock .timeUntilAutoLock settings shared
    , hasSeenTutorial = settings.hasSeenTutorial
    , seenNews = ORSet.set settings.seenNews shared.seenNews
    }


notSeenNews : Settings -> List String
notSeenNews settings =
    Set.diff allVersions settings.seenNews
        |> Set.toList


markSeen : String -> Settings -> Settings
markSeen s opt =
    if Set.member s allVersions then
        { opt | seenNews = Set.insert s opt.seenNews }
    else
        opt


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


setDoneWithTutorial : Settings -> Settings
setDoneWithTutorial settings =
    { settings
        | hasSeenTutorial = True
        , seenNews =
            if settings.hasSeenTutorial then
                settings.seenNews
            else
                -- Only set this if we have just seen the tutorial for the first time.
                --  => first time users don't have to read the update news, since it's not
                --     relevant to them.
                allVersions
    }


setAllowLevel1 : Bool -> Settings -> Settings
setAllowLevel1 b o =
    { o | allowLevel1 = b }


minSecurityLevel : SharedSettings -> Int
minSecurityLevel options =
    if (get options).allowLevel1 then
        1
    else
        2


init : Seed -> SharedSettings
init seed =
    { allowLevel1 = Nothing
    , timeUntilAutoLock = Nothing
    , hasSeenTutorial = False
    , seenNews = ORSet.init seed
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
merge other my =
    { allowLevel1 = mergeMaybe other.allowLevel1 my.allowLevel1
    , timeUntilAutoLock = mergeMaybe other.timeUntilAutoLock my.timeUntilAutoLock
    , hasSeenTutorial = other.hasSeenTutorial || my.hasSeenTutorial
    , seenNews = ORSet.merge other.seenNews my.seenNews
    }


defaults : Settings
defaults =
    { allowLevel1 = False, timeUntilAutoLock = 10 * Time.minute, hasSeenTutorial = False, seenNews = Set.empty }


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
            , [ ( "hasSeenTutorial", JE.bool opt.hasSeenTutorial )
              , ( "seenNews", ORSet.encode opt.seenNews )
              ]
            ]


decoder : Seed -> Decoder SharedSettings
decoder seed =
    JD.decode (\al1 t ht sn -> { allowLevel1 = al1, timeUntilAutoLock = t, hasSeenTutorial = ht, seenNews = sn })
        |> optional "allowLevel1" (TimestampedVersionRegister.decoder JD.bool |> JD.map Just) Nothing
        |> optional "timeUntilAutoLock" (TimestampedVersionRegister.decoder JD.float |> JD.map Just) Nothing
        |> optional "hasSeenTutorial" JD.bool False
        |> optional "seenNews" (ORSet.decoder seed) (ORSet.init seed)
