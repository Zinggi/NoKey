module SyncData exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Random.Pcg as Random exposing (Seed)
import Time exposing (Time)


--

import Helper exposing (decodeTuple, decodeTuple2, encodeTuple, encodeTuple2, encodeSet, decodeSet)
import SecretSharing
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import Crdt.VClock as VClock exposing (VClock)


{-| TODO: seperate private and true sync data, e.g. syncData we receive is different to our own data
-}
type alias SyncData =
    { knownIds : ORDict String (SingleVersionRegister String)

    {- Dict (SiteName, UserName) (Dict DeviceID Share)
       TODO: the dict should contain encryptedShares instead of the shares directly
    -}
    , savedSites : ORDict ( String, String ) (TimestampedVersionRegister SiteMeta)
    , version : VClock

    -- private data
    , id : String
    , seed : Seed
    , synchedWith : Dict String VClock
    , myShares : Dict ( String, String ) SecretSharing.Share
    }


type alias SiteMeta =
    { sharesForOthers : Dict String SecretSharing.Share
    , requiredParts : Int
    }


init : Seed -> String -> SyncData
init seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , synchedWith = Dict.empty
    , version = VClock.init
    , id = uuid
    , seed = seed
    , savedSites = ORDict.init seed
    , myShares = Dict.empty
    }


isKnownId : String -> SyncData -> Bool
isKnownId id sync =
    ORDict.get sync.knownIds |> Dict.member id


knownIds : SyncData -> List String
knownIds sync =
    ORDict.get sync.knownIds |> Dict.keys


knownOtherIds : SyncData -> List String
knownOtherIds sync =
    ORDict.get sync.knownIds |> Dict.remove sync.id |> Dict.keys


incrementVersion : SyncData -> SyncData
incrementVersion sync =
    -- TODO: we only need to increment at most one higher than any of our synchedWith versions
    -- (e.g. we don't need to increment if we are already higher or concurrent to all synchedWith)
    { sync | version = VClock.increment sync.id sync.version }


removeDevice : String -> SyncData -> SyncData
removeDevice uuid sync =
    incrementVersion { sync | knownIds = ORDict.remove uuid sync.knownIds }


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    let
        myName =
            ORDict.get sync.knownIds |> Dict.get sync.id |> Maybe.map Tuple.second |> Maybe.withDefault ""

        newKnownIds =
            ORDict.reset sync.knownIds |> ORDict.insert sync.id (SingleVersionRegister.init myName)
    in
        incrementVersion { sync | knownIds = newKnownIds }


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    incrementVersion { sync | knownIds = ORDict.update sync.id (SingleVersionRegister.update newName) sync.knownIds }


insertSite : Time -> Int -> String -> String -> Dict String SecretSharing.Share -> SyncData -> SyncData
insertSite timestamp reqParts siteName userName shares sync =
    let
        ( myShares, sharesForOthers ) =
            case Dict.get sync.id shares of
                Just share ->
                    ( Dict.insert ( siteName, userName ) share sync.myShares, Dict.remove sync.id shares )

                Nothing ->
                    Debug.log "This should never happen, but there is a save default, so we don't crash" <|
                        ( sync.myShares, shares )
    in
        incrementVersion
            { sync
                | savedSites =
                    ORDict.insert ( siteName, userName )
                        (TimestampedVersionRegister.init timestamp { sharesForOthers = sharesForOthers, requiredParts = reqParts })
                        sync.savedSites
                , myShares = myShares
            }


{-| mapSavedSites (\siteName userName hasShare -> ..)
-}
mapSavedSites : (String -> String -> Int -> Maybe SecretSharing.Share -> a) -> SyncData -> List a
mapSavedSites f sync =
    (ORDict.getWith TimestampedVersionRegister.get sync.savedSites)
        |> Dict.foldl
            (\( siteName, userName ) value acc ->
                f siteName userName value.requiredParts (Dict.get ( siteName, userName ) sync.myShares) :: acc
            )
            []


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA
-}
merge : Time -> SyncData -> SyncData -> SyncData
merge timestamp other my =
    let
        newSavedSites =
            ORDict.merge TimestampedVersionRegister.merge
                other.savedSites
                my.savedSites

        ( myShares, sharesForOthers ) =
            getMyShares my.id my.myShares (ORDict.getWith TimestampedVersionRegister.get newSavedSites)
    in
        { my
            | knownIds =
                ORDict.merge SingleVersionRegister.merge other.knownIds my.knownIds
            , savedSites = ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp) sharesForOthers newSavedSites
            , myShares = myShares
            , version = VClock.merge other.version my.version
            , synchedWith = Dict.insert other.id other.version my.synchedWith
        }


getMyShares :
    String
    -> Dict ( String, String ) SecretSharing.Share
    -> Dict ( String, String ) SiteMeta
    -> ( Dict ( String, String ) SecretSharing.Share, Dict ( String, String ) SiteMeta )
getMyShares id myOldShares savedSites =
    Dict.foldl
        (\( siteName, userName ) meta ( myShares, other ) ->
            case Dict.get id meta.sharesForOthers of
                Just share ->
                    ( Dict.insert ( siteName, userName ) share myShares
                    , Dict.insert ( siteName, userName ) { meta | sharesForOthers = Dict.remove id meta.sharesForOthers } other
                    )

                Nothing ->
                    ( myShares, other )
        )
        ( myOldShares, savedSites )
        savedSites


syncWithOthers : SyncData -> ( List String, SyncData )
syncWithOthers sync =
    let
        contactSet =
            List.foldl
                (\id l ->
                    case Dict.get id sync.synchedWith of
                        Just v ->
                            if VClock.isBeforeOrEqual sync.version v then
                                l
                            else
                                id :: l

                        Nothing ->
                            id :: l
                )
                []
                (knownOtherIds sync)
    in
        ( contactSet, { sync | synchedWith = List.foldl (\id -> Dict.insert id sync.version) sync.synchedWith contactSet } )


decoder : String -> Decoder SyncData
decoder id =
    JD.map3
        (\knownIds savedSites version ->
            { id = id
            , knownIds = knownIds
            , savedSites = savedSites
            , version = version
            , myShares = Dict.empty
            , synchedWith = Dict.empty
            , seed = Random.initialSeed 0
            }
        )
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder JD.string))
        (JD.field "savedSites" <|
            ORDict.decoder2 (decodeTuple JD.string)
                (TimestampedVersionRegister.decoder siteMetaDecoder)
        )
        (JD.field "version" VClock.decoder)


completeDecoder : Decoder SyncData
completeDecoder =
    JD.map7
        (\id knownIds savedSites myShares synchedWith seed version ->
            { id = id
            , knownIds = knownIds
            , savedSites = savedSites
            , version = version
            , myShares = myShares
            , synchedWith = synchedWith
            , seed = seed
            }
        )
        (JD.field "id" JD.string)
        (JD.field "knownIds" <| ORDict.completeDecoder (SingleVersionRegister.decoder JD.string))
        (JD.field "savedSites" <|
            ORDict.completeDecoder2 (decodeTuple JD.string)
                (TimestampedVersionRegister.decoder siteMetaDecoder)
        )
        (JD.field "myShares" <| JD.dict2 (decodeTuple JD.string) SecretSharing.shareDecoder)
        (JD.field "synchedWith" <| JD.dict VClock.decoder)
        (JD.field "seed" Random.fromJson)
        (JD.field "version" VClock.decoder)


siteMetaDecoder : Decoder SiteMeta
siteMetaDecoder =
    JD.map2 SiteMeta
        (JD.field "sharesForOthers" <| JD.dict SecretSharing.shareDecoder)
        (JD.field "requiredParts" JD.int)


encode : SyncData -> Value
encode s =
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.knownIds )
        , ( "savedSites"
          , ORDict.encode2
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode encodeSiteMeta)
                s.savedSites
          )
        , ( "version", VClock.encode s.version )
        ]


encodeComplete : SyncData -> Value
encodeComplete s =
    JE.object
        [ ( "knownIds", ORDict.encodeComplete identity (SingleVersionRegister.encode JE.string) s.knownIds )
        , ( "id", JE.string s.id )
        , ( "savedSites"
          , ORDict.encodeComplete
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode encodeSiteMeta)
                s.savedSites
          )
        , ( "myShares", JE.dict (\t -> encodeTuple JE.string t |> JE.encode 0) (SecretSharing.encodeShare) s.myShares )
        , ( "synchedWith", JE.dict identity VClock.encode s.synchedWith )
        , ( "seed", Random.toJson s.seed )
        , ( "version", VClock.encode s.version )
        ]


encodeSiteMeta : SiteMeta -> Value
encodeSiteMeta meta =
    JE.object
        [ ( "sharesForOthers", JE.dict identity SecretSharing.encodeShare meta.sharesForOthers )
        , ( "requiredParts", JE.int meta.requiredParts )
        ]
