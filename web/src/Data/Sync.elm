module Data.Sync exposing (..)

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


{-| This represents the data that is shared + all the metadata we need to sync this to others + our own private shares
-}
type alias SyncData =
    { shared : SharedData

    -- private data
    , id : String
    , synchedWith : Dict String VClock
    , myShares : Dict ( String, String ) SecretSharing.Share
    , seed : Seed
    }


{-| the data we receive on a sync update
-}
type alias OtherSharedData =
    { id : String, shared : SharedData }


{-| the data we want to share across all known devices
-}
type alias SharedData =
    { -- TODO: add device type, e.g. android, browser, extension, ...
      knownIds : ORDict String (SingleVersionRegister String)

    {- Dict (SiteName, UserName) SiteMeta
       TODO: the dict should contain encryptedShares instead of the shares directly
    -}
    , savedSites : ORDict ( String, String ) (TimestampedVersionRegister SiteMeta)
    , version : VClock
    }


type alias SiteMeta =
    { sharesForOthers : Dict String SecretSharing.Share
    , requiredParts : Int

    -- TODO: add a hash of the password, so that we can tell when loging in to a site,
    -- wheater we have a new password or the old one.
    -- => problem: hash contains info about password! more on notes...
    }


init : Seed -> String -> SyncData
init seed uuid =
    { shared = initShared seed uuid
    , synchedWith = Dict.empty
    , id = uuid
    , seed = seed
    , myShares = Dict.empty
    }


initShared : Seed -> String -> SharedData
initShared seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , savedSites = ORDict.init seed
    , version = VClock.init
    }


getAccountsForSite : String -> SyncData -> List String
getAccountsForSite site sync =
    savedSites sync
        |> Dict.filter (\( siteName, userName ) value -> site == siteName)
        |> Dict.keys
        |> List.map (\( siteName, userName ) -> userName)


updateShared : (SharedData -> SharedData) -> SyncData -> SyncData
updateShared f sync =
    { sync
        | shared =
            f sync.shared
                |> (\s -> { s | version = VClock.increment sync.id s.version })
    }


isKnownId : String -> SyncData -> Bool
isKnownId id sync =
    ORDict.get sync.shared.knownIds |> Dict.member id


savedSites : SyncData -> Dict ( String, String ) SiteMeta
savedSites sync =
    ORDict.getWith TimestampedVersionRegister.get sync.shared.savedSites


knownDevices : SyncData -> Dict String String
knownDevices sync =
    ORDict.getWith SingleVersionRegister.get sync.shared.knownIds


knownIds : SyncData -> List String
knownIds sync =
    ORDict.get sync.shared.knownIds |> Dict.keys


knownOtherIds : SyncData -> List String
knownOtherIds sync =
    ORDict.get sync.shared.knownIds |> Dict.remove sync.id |> Dict.keys


removeDevice : String -> SyncData -> SyncData
removeDevice uuid sync =
    updateShared (\s -> { s | knownIds = ORDict.remove uuid s.knownIds }) sync


getName : SyncData -> String
getName sync =
    knownDevices sync |> Dict.get sync.id |> Maybe.withDefault ""


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    -- clear all data
    (init sync.seed sync.id)
        -- keep version history of this device
        |> updateShared (\s -> { s | knownIds = ORDict.resetExceptOne sync.id sync.shared.knownIds })


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    updateShared (\s -> { s | knownIds = ORDict.update sync.id (SingleVersionRegister.update newName) s.knownIds }) sync


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

        updateEntry fn =
            fn sync.id timestamp { sharesForOthers = sharesForOthers, requiredParts = reqParts }
    in
        updateShared
            (\s ->
                { s
                    | savedSites =
                        ORDict.updateOrInsert ( siteName, userName )
                            (updateEntry TimestampedVersionRegister.set)
                            (updateEntry TimestampedVersionRegister.init)
                            s.savedSites
                }
            )
            { sync | myShares = myShares }


deletePassword : ( String, String ) -> SyncData -> SyncData
deletePassword key sync =
    -- TODO: also remove my shares + tell others to delete theirs?
    -- Or better, link the datastructures to automatically remove them
    updateShared (\s -> { s | savedSites = ORDict.remove key s.savedSites }) sync


{-| mapSavedSites (\siteName userName hasShare -> ..)
-}
mapSavedSites : (String -> String -> Int -> Maybe SecretSharing.Share -> a) -> SyncData -> List a
mapSavedSites f sync =
    (savedSites sync)
        |> Dict.foldl
            (\( siteName, userName ) value acc ->
                f siteName userName value.requiredParts (Dict.get ( siteName, userName ) sync.myShares) :: acc
            )
            []


getSavedSite : ( String, String ) -> SyncData -> Maybe ( Int, Maybe SecretSharing.Share )
getSavedSite key sync =
    (savedSites sync)
        |> Dict.get key
        |> Maybe.map (\v -> ( v.requiredParts, Dict.get key sync.myShares ))


getPasswordHashFor : String -> String -> SyncData -> Maybe String
getPasswordHashFor siteName userName sync =
    savedSites sync
        |> Dict.get ( siteName, userName )
        |> Maybe.map ({- TODO: should retrieve password hash, see above, there are some problems with this approach -} always "TODO")


merge : Time -> OtherSharedData -> SyncData -> SyncData
merge timestamp other my =
    let
        newSavedSites =
            ORDict.merge TimestampedVersionRegister.merge
                other.shared.savedSites
                my.shared.savedSites

        ( myShares, sharesForOthers ) =
            getMyShares my.id my.myShares (ORDict.getWith TimestampedVersionRegister.get newSavedSites)
    in
        { my
            | shared =
                { knownIds = ORDict.merge SingleVersionRegister.merge other.shared.knownIds my.shared.knownIds
                , savedSites = ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp) sharesForOthers newSavedSites
                , version = VClock.merge other.shared.version my.shared.version
                }
            , myShares = myShares
        }
            |> receiveVersion other.id other.shared.version


receiveVersion : String -> VClock -> SyncData -> SyncData
receiveVersion from version sync =
    { sync | synchedWith = Dict.insert from version sync.synchedWith }


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


syncWithOthers : SyncData -> ( ( List String, List String ), SyncData )
syncWithOthers sync =
    let
        contactSets =
            List.foldl
                (\id ( needMine, needTheirs ) ->
                    case Dict.get id sync.synchedWith of
                        Just v ->
                            if VClock.isBefore sync.shared.version v then
                                ( needMine, id :: needTheirs )
                            else if VClock.isEqual sync.shared.version v then
                                ( needMine, needTheirs )
                            else
                                ( id :: needMine, needTheirs )

                        Nothing ->
                            ( id :: needMine, needTheirs )
                )
                ( [], [] )
                (knownOtherIds sync)
    in
        ( contactSets
          -- assume it gets delivered
        , { sync | synchedWith = List.foldl (\id -> Dict.insert id sync.shared.version) sync.synchedWith (Tuple.first contactSets) }
        )


decoder : String -> Decoder OtherSharedData
decoder id =
    JD.map3
        (\knownIds savedSites version ->
            { id = id
            , shared =
                { knownIds = knownIds
                , savedSites = savedSites
                , version = version
                }
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
        (\id knownIds savedSites myShares synchedWith version seed ->
            { id = id
            , shared =
                { knownIds = knownIds
                , savedSites = savedSites
                , version = version
                }
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
        (JD.field "version" VClock.decoder)
        (JD.field "seed" Random.fromJson)


siteMetaDecoder : Decoder SiteMeta
siteMetaDecoder =
    JD.map2 SiteMeta
        (JD.field "sharesForOthers" <| JD.dict SecretSharing.shareDecoder)
        (JD.field "requiredParts" JD.int)


encode : SyncData -> Value
encode s =
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.shared.knownIds )
        , ( "savedSites"
          , ORDict.encode2
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode encodeSiteMeta)
                s.shared.savedSites
          )
        , ( "version", VClock.encode s.shared.version )
        ]


encodeComplete : SyncData -> Value
encodeComplete s =
    JE.object
        [ ( "knownIds", ORDict.encodeComplete identity (SingleVersionRegister.encode JE.string) s.shared.knownIds )
        , ( "id", JE.string s.id )
        , ( "savedSites"
          , ORDict.encodeComplete
                (\t -> encodeTuple JE.string t |> JE.encode 0)
                (TimestampedVersionRegister.encode encodeSiteMeta)
                s.shared.savedSites
          )
        , ( "myShares", JE.dict (\t -> encodeTuple JE.string t |> JE.encode 0) (SecretSharing.encodeShare) s.myShares )
        , ( "synchedWith", JE.dict identity VClock.encode s.synchedWith )
        , ( "version", encodeVersion s )
        , ( "seed", Random.toJson s.seed )
        ]


encodeVersion : SyncData -> Value
encodeVersion sync =
    VClock.encode sync.shared.version


encodeSiteMeta : SiteMeta -> Value
encodeSiteMeta meta =
    JE.object
        [ ( "sharesForOthers", JE.dict identity SecretSharing.encodeShare meta.sharesForOthers )
        , ( "requiredParts", JE.int meta.requiredParts )
        ]
