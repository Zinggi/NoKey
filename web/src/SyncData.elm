module SyncData exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Random.Pcg as Random exposing (Seed)
import Time exposing (Time)


--

import Helper exposing (decodeTuple, decodeTuple2, encodeTuple, encodeTuple2)
import SecretSharing
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)


type alias SyncData =
    { knownIds : ORDict String (SingleVersionRegister String)

    {- Dict (SiteName, UserName) (Dict DeviceID Share)
       TODO: the dict should contain encryptedShares instead of the shares directly
    -}
    , savedSites : ORDict ( String, String ) (TimestampedVersionRegister (Dict String SecretSharing.Share))

    -- private data
    , id : String
    , seed : Seed
    , synchedWith : Set String
    , myShares : Dict ( String, String ) SecretSharing.Share
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


knownIds : SyncData -> List String
knownIds sync =
    ORDict.get sync.knownIds |> Dict.keys


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


pairedWith : String -> SyncData -> SyncData -> SyncData
pairedWith uuid hisSync mySync =
    case Dict.get uuid (ORDict.get hisSync.knownIds) of
        Just v ->
            { mySync | knownIds = ORDict.insert uuid v mySync.knownIds }

        Nothing ->
            mySync


insertSite : Time -> String -> String -> Dict String SecretSharing.Share -> SyncData -> SyncData
insertSite timestamp siteName userName shares sync =
    let
        ( myShares, sharesForOthers ) =
            case Dict.get sync.id shares of
                Just share ->
                    ( Dict.insert ( siteName, userName ) share sync.myShares, Dict.remove sync.id shares )

                Nothing ->
                    Debug.log "This should never happen, but there is a save default, so don't crash" <|
                        ( sync.myShares, shares )
    in
        { sync
            | savedSites = ORDict.insert ( siteName, userName ) (TimestampedVersionRegister.init timestamp sharesForOthers) sync.savedSites
            , synchedWith = Set.empty
            , myShares = myShares
        }


{-| mapSavedSites (\siteName userName hasShare -> ..)
-}
mapSavedSites : (String -> String -> Bool -> a) -> SyncData -> List a
mapSavedSites f sync =
    (ORDict.get sync.savedSites)
        |> Dict.foldl
            (\( siteName, userName ) value acc ->
                f siteName userName (Dict.member ( siteName, userName ) sync.myShares) :: acc
            )
            []


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA

TODO: upon receive a savedSite for me, take my share out into myShares
and remove it from the savedSites

-}
merge : Time -> SyncData -> SyncData -> SyncData
merge timestamp other my =
    adjustSynchedWith other
        my
        { my
            | knownIds =
                ORDict.merge SingleVersionRegister.merge other.knownIds my.knownIds
            , savedSites =
                ORDict.merge TimestampedVersionRegister.merge
                    other.savedSites
                    my.savedSites
        }


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
                (TimestampedVersionRegister.decoder (JD.dict SecretSharing.shareDecoder))
        )


encode : SyncData -> Value
encode s =
    -- TODO: don't encode ID
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.knownIds )
        , ( "id", JE.string s.id )
        , ( "savedSites"
          , ORDict.encode2
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode (JE.dict identity SecretSharing.encodeShare))
                s.savedSites
          )
        ]
