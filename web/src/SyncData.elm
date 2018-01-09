module SyncData exposing (..)

import Set exposing (Set)
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


type alias SyncData =
    { knownIds : ORDict String (SingleVersionRegister String)

    {- Dict (SiteName, UserName) (Dict DeviceID Share)
       TODO: the dict should contain encryptedShares instead of the shares directly
    -}
    , savedSites : ORDict ( String, String ) (TimestampedVersionRegister SiteMeta)

    -- private data
    , id : String
    , seed : Seed
    , synchedWith : Set String
    , myShares : Dict ( String, String ) SecretSharing.Share
    }


type alias SiteMeta =
    { sharesForOthers : Dict String SecretSharing.Share
    , requiredParts : Int
    }


init : Seed -> String -> SyncData
init seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , synchedWith = Set.empty
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


removeDevice : String -> SyncData -> SyncData
removeDevice uuid sync =
    { sync | knownIds = ORDict.remove uuid sync.knownIds, synchedWith = Set.empty }


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    let
        myName =
            ORDict.get sync.knownIds |> Dict.get sync.id |> Maybe.map Tuple.second |> Maybe.withDefault ""
    in
        { sync | knownIds = ORDict.reset sync.knownIds |> ORDict.insert sync.id (SingleVersionRegister.init myName), synchedWith = Set.empty }


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    { sync | knownIds = ORDict.update sync.id (SingleVersionRegister.update newName) sync.knownIds, synchedWith = Set.empty }


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
        { sync
            | savedSites =
                ORDict.insert ( siteName, userName )
                    (TimestampedVersionRegister.init timestamp { sharesForOthers = sharesForOthers, requiredParts = reqParts })
                    sync.savedSites
            , synchedWith = Set.empty
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
        adjustSynchedWith other
            my
            { my
                | knownIds =
                    ORDict.merge SingleVersionRegister.merge other.knownIds my.knownIds
                , savedSites = ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp) sharesForOthers newSavedSites
                , myShares = myShares
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


adjustSynchedWith : SyncData -> SyncData -> SyncData -> SyncData
adjustSynchedWith other myOld myNew =
    -- TODO: incorporate savedSites
    -- TODO: refactor to use a version vector
    -- e.g. have a single version vector for the version of the data + a dict of versions to keep track of who knows what.
    { myNew
        | synchedWith =
            (if not <| ORDict.equal myNew.knownIds myOld.knownIds then
                -- I got some new knowledge, assume nobody knows what I know.
                -- TODO: updating others of the new state that I just received should not be my business,
                -- it would be more efficient if I'd assume that others also get this update.
                Set.empty
             else
                (if ORDict.equal other.knownIds myNew.knownIds then
                    -- I didn't receive any new knowledge and the other does know as much as I do, so his information doesn't change anything.
                    Set.insert other.id myNew.synchedWith
                 else
                    -- I didn't receive any new info and the other knows less than I, so let him know in my next update
                    Set.remove other.id myNew.synchedWith
                )
            )
                |> (\sWith ->
                        if ORDict.equal myNew.knownIds other.knownIds then
                            Set.insert other.id sWith
                        else
                            sWith
                   )
    }


getContactSet : SyncData -> Set String
getContactSet sync =
    Set.diff (ORDict.get sync.knownIds |> Dict.keys |> Set.fromList) (Set.insert sync.id sync.synchedWith)


updateSynchedWith : Set String -> SyncData -> SyncData
updateSynchedWith s sync =
    { sync | synchedWith = Set.union s sync.synchedWith }


resetSynchedWith : SyncData -> SyncData
resetSynchedWith sync =
    { sync | synchedWith = Set.empty }


decoder : Decoder SyncData
decoder =
    JD.map3
        (\id knownIds savedSites ->
            { id = id
            , knownIds = knownIds
            , savedSites = savedSites
            , myShares = Dict.empty
            , synchedWith = Set.empty
            , seed = Random.initialSeed 0
            }
        )
        -- TODO: don't decode id, get it from the sender
        (JD.field "id" JD.string)
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder JD.string))
        (JD.field "savedSites" <|
            ORDict.decoder2 (decodeTuple JD.string)
                (TimestampedVersionRegister.decoder siteMetaDecoder)
        )


completeDecoder : Decoder SyncData
completeDecoder =
    JD.map6
        (\id knownIds savedSites myShares synchedWith seed ->
            { id = id
            , knownIds = knownIds
            , savedSites = savedSites
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
        (JD.field "synchedWith" <| decodeSet JD.string)
        (JD.field "seed" Random.fromJson)


siteMetaDecoder : Decoder SiteMeta
siteMetaDecoder =
    JD.map2 SiteMeta
        (JD.field "sharesForOthers" <| JD.dict SecretSharing.shareDecoder)
        (JD.field "requiredParts" JD.int)


encode : SyncData -> Value
encode s =
    -- TODO: don't encode ID
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.knownIds )
        , ( "id", JE.string s.id )
        , ( "savedSites"
          , ORDict.encode2
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode encodeSiteMeta)
                s.savedSites
          )
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
        , ( "synchedWith", encodeSet JE.string s.synchedWith )
        , ( "seed", Random.toJson s.seed )
        ]


encodeSiteMeta : SiteMeta -> Value
encodeSiteMeta meta =
    JE.object
        [ ( "sharesForOthers", JE.dict identity SecretSharing.encodeShare meta.sharesForOthers )
        , ( "requiredParts", JE.int meta.requiredParts )
        ]
