module SyncData exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random.Pcg as Random exposing (Seed)


--

import Helper exposing (decodeTuple, decodeTuple2, encodeTuple, encodeTuple2)
import SecretSharing
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.GSet as GSet exposing (GSet)
import Crdt.GCounter as GCounter exposing (GCounter)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)


type alias SyncData =
    { knownIds : ORDict String (SingleVersionRegister String)

    {- Dict (SiteName, UserName) (SecretVersionNumber, DevicesThatKnowAShare) -}
    , savedSites : ORDict ( String, String ) ( GCounter, GSet String )

    -- private data
    , id : String
    , synchedWith : Set String
    , shares : Dict ( String, String ) SecretSharing.Share
    , sharesForOthers : Dict ( String, String, String ) SecretSharing.Share
    }


init : Seed -> String -> SyncData
init seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , synchedWith = Set.empty
    , id = uuid
    , savedSites = ORDict.init seed
    , shares = Dict.empty
    , sharesForOthers = Dict.empty
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


insertSite : String -> String -> Dict String SecretSharing.Share -> SyncData -> SyncData
insertSite siteName userName shares sync =
    { sync
        | savedSites = ORDict.insert ( siteName, userName ) ( GCounter.init, GSet.init ) sync.savedSites
        , synchedWith = Set.empty
        , shares =
            case Dict.get sync.id shares of
                Just share ->
                    Dict.insert ( siteName, userName ) share sync.shares

                Nothing ->
                    sync.shares

        -- TODO: this erases old shares
        , sharesForOthers =
            Dict.foldl
                (\key value acc ->
                    Dict.insert ( key, siteName, userName ) value acc
                )
                sync.sharesForOthers
                shares
    }


{-| mapSavedSites (\siteName userName hasShare -> ..)
-}
mapSavedSites : (String -> String -> Bool -> a) -> SyncData -> List a
mapSavedSites f sync =
    (ORDict.get sync.savedSites)
        |> Dict.foldl
            (\( siteName, userName ) value acc ->
                f siteName userName (Dict.member ( siteName, userName ) sync.shares) :: acc
            )
            []


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA
-}
merge : SyncData -> SyncData -> SyncData
merge other my =
    adjustSynchedWith other
        my
        { my
            | knownIds =
                ORDict.merge SingleVersionRegister.merge other.knownIds my.knownIds
            , savedSites =
                ORDict.merge
                    (\( aCount, aSet ) ( bCount, bSet ) ->
                        ( GCounter.merge aCount bCount, GSet.merge aSet bSet )
                    )
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
            , shares = Dict.empty
            , synchedWith = Set.empty
            , sharesForOthers = Dict.empty
            }
        )
        -- TODO: don't decode id, get it from the sender
        (JD.field "id" JD.string)
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder JD.string))
        (JD.field "savedSites" <| ORDict.decoder2 (decodeTuple JD.string) (decodeTuple2 GCounter.decoder GSet.decoder))


encode : SyncData -> Value
encode s =
    -- TODO: don't encode ID
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.knownIds )
        , ( "id", JE.string s.id )
        , ( "savedSites", ORDict.encode2 (\t -> encodeTuple JE.string t |> JE.encode 0) (encodeTuple2 GCounter.encode GSet.encode) s.savedSites )
        ]
