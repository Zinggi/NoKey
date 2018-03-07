module Data.Sync exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
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
    , myShares : Dict GroupId SecretSharing.Share
    , passwordStash : Dict AccountId ( GroupId, Password )
    , seed : Seed
    }


type alias Password =
    String


{-| (SecurityLevel, CreatorId)
-}
type alias GroupId =
    ( Int, String )


{-| the data we receive on a sync update
-}
type alias OtherSharedData =
    { id : String, shared : SharedData }


{-| the data we want to share across all known devices
-}
type alias SharedData =
    { -- TODO: add device type, e.g. android, browser, extension, ...
      knownIds : ORDict String (SingleVersionRegister String)

    -- These shares will be taken out one by one by the device whose id matches the id
    -- in the second dict
    -- TODO: here we should store encrypted shares with e.g. RSA
    , sharesToDistribute : ORDict GroupId (ORDict String (TimestampedVersionRegister SecretSharing.Share))

    -- here we store all passwords, encrypted with the group password
    , passwords : ORDict AccountId (TimestampedVersionRegister ( GroupId, EncryptedPassword ))
    , version : VClock
    }


encryptedPasswords : SyncData -> Dict AccountId ( GroupId, EncryptedPassword )
encryptedPasswords sync =
    ORDict.getWith TimestampedVersionRegister.get sync.shared.passwords


accounts : SyncData -> Dict AccountId GroupId
accounts sync =
    encryptedPasswords sync
        |> Dict.map (\key ( groupId, _ ) -> groupId)


getShare : GroupId -> SyncData -> Maybe SecretSharing.Share
getShare groupId sync =
    Dict.get groupId sync.myShares


type alias AccountId =
    -- siteName, userName
    ( String, String )


type EncryptedPassword
    = -- TODO: encrypted with http://package.elm-lang.org/packages/billstclair/elm-crypto-string/latest
      EncryptedPassword String


type alias GroupPassword =
    String


encryptPassword : GroupPassword -> Password -> EncryptedPassword
encryptPassword groupPw pw =
    Debug.crash "TODO"


decryptPassword : GroupPassword -> EncryptedPassword -> Password
decryptPassword groupPw encryptedPw =
    Debug.crash "TODO"


init : Seed -> String -> SyncData
init seed uuid =
    { shared = initShared seed uuid
    , synchedWith = Dict.empty
    , id = uuid
    , seed = seed
    , myShares = Dict.empty
    , passwordStash = Dict.empty
    }


initShared : Seed -> String -> SharedData
initShared seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , sharesToDistribute = ORDict.init seed
    , passwords = ORDict.init seed
    , version = VClock.init
    }


getAccountsForSite : String -> SyncData -> List ( String, GroupId )
getAccountsForSite site sync =
    accounts sync
        |> Dict.filterMap
            (\( siteName, userName ) groupId ->
                if site == siteName then
                    Just ( userName, groupId )
                else
                    Nothing
            )
        |> Dict.values


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


{-| returns Dict Id (Name, IdPart)
the IdPart is used to distinguish devices with the same name
-}
knownDevices : SyncData -> Dict String ( String, String )
knownDevices sync =
    ORDict.getWith SingleVersionRegister.get sync.shared.knownIds
        |> Dict.toList
        |> Dict.groupBy Tuple.second
        -- Now we have a Dict Name (List (Id, Name))
        |> Dict.map
            (\name idsToName ->
                List.map Tuple.first idsToName
                    |> (\ids -> List.map2 (,) ids (Helper.findNonEqualBeginning ids))
            )
        -- Now we have a Dict Name (List (Id, NecessairyBeginningsOfId))
        |> Dict.foldl
            (\name idsToIdPart acc ->
                Dict.fromList idsToIdPart
                    |> Dict.map (\id idPart -> ( name, idPart ))
                    |> Dict.union acc
            )
            Dict.empty


knownIds : SyncData -> List String
knownIds sync =
    ORDict.get sync.shared.knownIds |> Dict.keys


knownOtherIds : SyncData -> List String
knownOtherIds sync =
    ORDict.get sync.shared.knownIds |> Dict.remove sync.id |> Dict.keys


removeDevice : String -> SyncData -> SyncData
removeDevice uuid sync =
    updateShared (\s -> { s | knownIds = ORDict.remove uuid s.knownIds }) sync


getName : SyncData -> ( String, String )
getName sync =
    knownDevices sync |> Dict.get sync.id |> Maybe.withDefault ( "", "" )


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    -- clear all data
    (init sync.seed sync.id)
        -- keep version history of this device
        |> updateShared (\s -> { s | knownIds = ORDict.resetExceptOne sync.id sync.shared.knownIds })


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    updateShared (\s -> { s | knownIds = ORDict.update sync.id (SingleVersionRegister.update newName) s.knownIds }) sync


insertToStash : AccountId -> GroupId -> Password -> SyncData -> SyncData
insertToStash accountId groupId pw sync =
    { sync | passwordStash = Dict.insert accountId ( groupId, pw ) sync.passwordStash }


{-| TODO: Insert a site. To call this we need to know the group password or
on first invocation generate a new groupId + password
-}
insertSite : Time -> GroupPassword -> AccountId -> GroupId -> Password -> SyncData -> SyncData
insertSite timestamp groupPw accountId groupId pw sync =
    let
        encPw =
            encryptPassword groupPw pw

        updateFn fn =
            fn sync.id timestamp ( groupId, encPw )
    in
        sync
            |> updateShared
                (\s ->
                    { s
                        | passwords =
                            ORDict.updateOrInsert accountId
                                (updateFn TimestampedVersionRegister.set)
                                (updateFn TimestampedVersionRegister.init)
                                s.passwords
                    }
                )



-- ORDict.updateOrInsert ( siteName, userName )
--     (updateEntry TimestampedVersionRegister.set)
--     (updateEntry TimestampedVersionRegister.init)
--     s.savedSites


deletePassword : AccountId -> SyncData -> SyncData
deletePassword key sync =
    -- TODO: also remove my shares + tell others to delete theirs?
    -- Or better, link the datastructures to automatically remove them
    -- TODO: should this even be allowed without unlocking the group??
    updateShared (\s -> { s | passwords = ORDict.remove key s.passwords }) sync


mapAccounts : (AccountId -> GroupId -> Maybe SecretSharing.Share -> a) -> SyncData -> List a
mapAccounts f sync =
    (accounts sync)
        |> Dict.foldl
            (\accountId groupId acc ->
                f accountId groupId (Dict.get groupId sync.myShares) :: acc
            )
            []


hasPasswordFor : AccountId -> SyncData -> Bool
hasPasswordFor key sync =
    encryptedPasswords sync
        |> Dict.member key


merge : Time -> OtherSharedData -> SyncData -> SyncData
merge timestamp other my =
    let
        newSharesToDistribute =
            ORDict.merge (ORDict.merge TimestampedVersionRegister.merge)
                other.shared.sharesToDistribute
                my.shared.sharesToDistribute

        newPasswords =
            ORDict.merge TimestampedVersionRegister.merge other.shared.passwords my.shared.passwords

        ( myShares, sharesForOthers ) =
            getMyShares my.id
                my.myShares
                (ORDict.getWith (ORDict.getWith TimestampedVersionRegister.get) newSharesToDistribute)
    in
        { my
            | shared =
                { knownIds = ORDict.merge SingleVersionRegister.merge other.shared.knownIds my.shared.knownIds
                , passwords = newPasswords
                , sharesToDistribute =
                    ORDict.updateWithDict (ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp))
                        sharesForOthers
                        newSharesToDistribute
                , version = VClock.merge other.shared.version my.shared.version
                }
            , myShares = myShares
        }
            |> receiveVersion other.id other.shared.version


receiveVersion : String -> VClock -> SyncData -> SyncData
receiveVersion from version sync =
    { sync | synchedWith = Dict.insert from version sync.synchedWith }


{-| take out my shares if there are any
-}
getMyShares :
    String
    -> Dict GroupId SecretSharing.Share
    -> Dict GroupId (Dict String SecretSharing.Share)
    -> ( Dict GroupId SecretSharing.Share, Dict GroupId (Dict String SecretSharing.Share) )
getMyShares id myOldShares sharesToDistribute =
    Dict.foldl
        (\( siteName, userName ) sharesForOthers ( myShares, other ) ->
            case Dict.get id sharesForOthers of
                Just share ->
                    ( Dict.insert ( siteName, userName ) share myShares
                    , Dict.insert ( siteName, userName ) (Dict.remove id sharesForOthers) other
                    )

                Nothing ->
                    ( myShares, other )
        )
        ( myOldShares, sharesToDistribute )
        sharesToDistribute


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


otherSharedDecoder : String -> Decoder OtherSharedData
otherSharedDecoder id =
    JD.field "dataVersion" JD.int
        |> JD.andThen
            (\v ->
                case v of
                    1 ->
                        decoderV1 id

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


sharedDecoder : Decoder SharedData
sharedDecoder =
    JD.map4
        (\knownIds passwords sharesToDistribute version ->
            { knownIds = knownIds
            , passwords = passwords
            , sharesToDistribute = sharesToDistribute
            , version = version
            }
        )
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder JD.string))
        (JD.field "passwords" <|
            ORDict.decoder2 accountIdDecoder
                (TimestampedVersionRegister.decoder (decodeTuple2 groupIdDecoder encryptedPasswordDecoder))
        )
        (JD.field "sharesToDistribute" <|
            ORDict.decoder2 groupIdDecoder
                (ORDict.decoder (TimestampedVersionRegister.decoder SecretSharing.shareDecoder))
        )
        (JD.field "version" VClock.decoder)


decoderV1 : String -> Decoder OtherSharedData
decoderV1 id =
    JD.field "shared" sharedDecoder
        |> JD.map (\shared -> { id = id, shared = shared })


encryptedPasswordDecoder : Decoder EncryptedPassword
encryptedPasswordDecoder =
    JD.map EncryptedPassword JD.string


groupIdDecoder : Decoder GroupId
groupIdDecoder =
    decodeTuple2 JD.int JD.string


accountIdDecoder : Decoder AccountId
accountIdDecoder =
    decodeTuple JD.string


completeDecoder : Decoder SyncData
completeDecoder =
    JD.field "dataVersion" JD.int
        |> JD.andThen
            (\v ->
                case v of
                    1 ->
                        completeDecoderV1

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


completeDecoderV1 : Decoder SyncData
completeDecoderV1 =
    JD.map6
        (\id shared myShares synchedWith passwordStash seed ->
            { id = id
            , shared = shared
            , myShares = myShares
            , synchedWith = synchedWith
            , passwordStash = passwordStash
            , seed = seed
            }
        )
        (JD.field "id" JD.string)
        (JD.field "shared" sharedDecoder)
        (JD.field "myShares" <| JD.dict2 groupIdDecoder SecretSharing.shareDecoder)
        (JD.field "synchedWith" <| JD.dict VClock.decoder)
        (JD.field "passwordStash" <| JD.dict2 accountIdDecoder (decodeTuple2 groupIdDecoder JD.string))
        (JD.field "seed" Random.fromJson)



-- (JD.field "passwords" <|
--     ORDict.decoder2 accountIdDecoder
--         (TimestampedVersionRegister.decoder (decodeTuple2 groupIdDecoder encryptedPasswordDecoder))
-- )
-- (JD.field "sharesToDistribute" <| ORDict.decoder2 groupIdDecoder (ORDict.decoder SecretSharing.shareDecoder))


encodeShared : SharedData -> Value
encodeShared shared =
    JE.object
        [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) shared.knownIds )
        , ( "passwords"
          , ORDict.encode2 encodeAccountId
                (TimestampedVersionRegister.encode (encodeTuple2 encodeGroupId encodeEncryptedPassword))
                shared.passwords
          )
        , ( "sharesToDistribute"
          , ORDict.encode2 encodeGroupId
                (ORDict.encode (TimestampedVersionRegister.encode SecretSharing.encodeShare))
                shared.sharesToDistribute
          )
        , ( "version", VClock.encode shared.version )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 1 )
        ]


encodeEncryptedPassword : EncryptedPassword -> Value
encodeEncryptedPassword (EncryptedPassword pw) =
    JE.string pw


encodeAccountId : AccountId -> Value
encodeAccountId id =
    encodeTuple JE.string id


encodeGroupId : GroupId -> Value
encodeGroupId id =
    encodeTuple2 JE.int JE.string id


encodeComplete : SyncData -> Value
encodeComplete s =
    JE.object
        [ ( "id", JE.string s.id )
        , ( "shared", encodeShared s.shared )
        , ( "myShares", JE.dict (encodeGroupId >> JE.encode 0) (SecretSharing.encodeShare) s.myShares )
        , ( "synchedWith", JE.dict identity VClock.encode s.synchedWith )
        , ( "passwordStash", JE.dict (encodeAccountId >> JE.encode 0) (encodeTuple2 encodeGroupId JE.string) s.passwordStash )
        , ( "seed", Random.toJson s.seed )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 1 )
        ]


encodeVersion : SyncData -> Value
encodeVersion sync =
    VClock.encode sync.shared.version
