module Data.Sync exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Set
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Random.Pcg as Random exposing (Seed)
import Random.Pcg.Extended as RandomE
import Random as RandomC
import Time exposing (Time)


--

import Helper exposing (decodeTuple, decodeTuple2, encodeTuple, encodeTuple2, encodeSet, decodeSet)
import SecretSharing
import AES
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import Crdt.VClock as VClock exposing (VClock)
import Data.RequestGroupPassword as Request exposing (Status, PasswordStatus)
import Data exposing (..)


{-| This represents the data that is shared + all the metadata we need to sync this to others + our own private shares
-}
type alias SyncData =
    { shared : SharedData

    -- private data
    , id : String
    , synchedWith : Dict String VClock
    , myShares : Dict GroupId SecretSharing.Share

    -- TODO: instead of keeping these two here, create a concept of a todolist.
    -- This can than be used for things like:
    --      Move Password for AccountId from (local) stash into GroupId
    --          => user needs to unlock GroupId
    --      Move Password for AccountId from GroupId1 into GroupId2
    --          => user needs to unlock both groups 1 and 2
    --      Create shares for Ids for GroupIds
    --          => user needs to unlock GroupIds
    , passwordStash : Dict AccountId ( GroupId, Password )
    , groupPasswordStash : Dict GroupId GroupPassword
    , seed : Seed

    -- ephemeral
    , groupPasswordRequestsState : Request.State
    }


clearStashes : GroupId -> SyncData -> SyncData
clearStashes groupId sync =
    { sync
        | groupPasswordStash = Dict.remove groupId sync.groupPasswordStash
        , passwordStash = Dict.filter (\accountId ( grId, _ ) -> grId /= groupId) sync.passwordStash
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

    -- These shares will be taken out one by one by the device whose id matches the id
    -- in the second dict
    -- TODO: here we should store encrypted shares with e.g. RSA
    -- TODO: we should also store who has a share for which pw
    --       This way we can:
    --          + TODO: remove a pw from the stash, once enough devices took their share
    --          + give the user a better understanding of how it works:
    --              - E.g. if the user clicks on a group in the passwords view, show which devices have a share
    --          + automatically calculate new shares on unlock group for devices
    --            that have no share yet and aren't in the to distribute list
    , sharesToDistribute : ORDict GroupId (TimestampedVersionRegister (Dict String SecretSharing.Share))

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


groups : SyncData -> List GroupId
groups sync =
    accounts sync
        |> Dict.foldl (\_ groupId acc -> Set.insert groupId acc) Set.empty
        |> Set.toList


getShare : GroupId -> SyncData -> Maybe SecretSharing.Share
getShare groupId sync =
    Dict.get groupId sync.myShares


init : Seed -> String -> SyncData
init seed uuid =
    { shared = initShared seed uuid
    , synchedWith = Dict.empty
    , id = uuid
    , seed = seed
    , myShares = Dict.empty
    , passwordStash = Dict.empty
    , groupPasswordStash = Dict.empty
    , groupPasswordRequestsState = Request.init
    }


initShared : Seed -> String -> SharedData
initShared seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , sharesToDistribute = ORDict.init seed
    , passwords = ORDict.init seed
    , version = VClock.init
    }


togglePassword : AccountId -> SyncData -> SyncData
togglePassword accountId sync =
    { sync
        | groupPasswordRequestsState =
            Request.togglePassword accountId
                (Dict.get accountId (encryptedPasswords sync))
                (Dict.get accountId sync.passwordStash |> Maybe.map Tuple.second)
                sync.groupPasswordRequestsState
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


{-| TODO: only increment if nececairry, e.g. if we aren't already newer than all the other devices
-}
updateShared : (SharedData -> SharedData) -> SyncData -> SyncData
updateShared f sync =
    { sync
        | shared =
            f sync.shared
                |> (\s -> { s | version = VClock.increment sync.id s.version })
    }


incrementIf : Bool -> SyncData -> SyncData
incrementIf b sync =
    if b then
        updateShared identity sync
    else
        sync


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


{-| The caller is expected to call Api.requestShare if the last part of the tuple is True.
-}
insertSite : Time -> RandomE.Seed -> AccountId -> GroupId -> Password -> SyncData -> ( SyncData, RandomE.Seed, Bool )
insertSite time seed accountId groupId pw sync =
    case Request.getGroupPassword groupId sync.groupPasswordRequestsState of
        -- if have group password then
        Just groupPw ->
            -- insert into passwords
            ( insertToStorage time groupPw accountId groupId pw sync, seed, False )

        Nothing ->
            let
                -- add password to stash
                newSync =
                    insertToStash accountId groupId pw sync

                ( level, _ ) =
                    groupId
            in
                -- if groupId exists then
                if List.member groupId (groups sync) then
                    ( updateGroupPasswordRequest (Request.waitFor groupId Nothing (getShare groupId sync)) newSync
                    , seed
                    , True
                    )
                else
                    let
                        -- generate group password
                        ( groupPw, seed2 ) =
                            RandomE.step Helper.groupPwGenerator seed

                        -- generate shares
                        ( genShares, seed3 ) =
                            SecretSharing.splitString
                                ( level
                                , knownIds sync |> List.length
                                )
                                groupPw
                                seed2

                        shares =
                            List.map2 (,) (knownIds sync) genShares
                                |> Dict.fromList

                        -- take out our share
                        ( myShares, sharesForOthers ) =
                            case Dict.get sync.id shares of
                                Just share ->
                                    ( Dict.insert groupId share sync.myShares, Dict.remove sync.id shares )

                                Nothing ->
                                    Debug.log "This should never happen, but there is a save default, so we don't crash" <|
                                        ( sync.myShares, shares )

                        newSync2 =
                            -- TODO: here we should encrypt the shares for the others
                            -- share with others
                            { newSync | myShares = myShares }
                                |> updateShared
                                    (\s ->
                                        { s
                                            | sharesToDistribute =
                                                ORDict.updateOrInsert groupId
                                                    (TimestampedVersionRegister.set sync.id time sharesForOthers)
                                                    (TimestampedVersionRegister.init sync.id time sharesForOthers)
                                                    s.sharesToDistribute
                                        }
                                    )
                                -- add groupPw to stash
                                |> (\s -> { s | groupPasswordStash = Dict.insert groupId groupPw s.groupPasswordStash })
                                -- store pw in passwords
                                |> insertToStorage time groupPw accountId groupId pw
                                -- add both groupPw and pw to cache
                                |> updateGroupPasswordRequest (Request.cacheAccountPw accountId pw False >> Request.cacheGroupPw groupId groupPw)
                    in
                        ( newSync2, seed3, False )


insertToStorage : Time -> GroupPassword -> AccountId -> GroupId -> Password -> SyncData -> SyncData
insertToStorage timestamp groupPw accountId groupId pw sync =
    let
        updateFn p fn =
            fn sync.id timestamp ( groupId, p )
    in
        case AES.encryptPassword timestamp groupPw pw of
            Ok encPw ->
                sync
                    |> updateShared
                        (\s ->
                            { s
                                | passwords =
                                    ORDict.updateOrInsert accountId
                                        (updateFn encPw TimestampedVersionRegister.set)
                                        (updateFn encPw TimestampedVersionRegister.init)
                                        s.passwords
                            }
                        )

            Err str ->
                -- TODO: probably just ignore error, don't crash!
                Debug.crash
                    ("Encrypting a password failed? But why???\n(groupPw, accountId, groupId, pw):\n"
                        ++ toString ( groupPw, accountId, groupId, pw )
                    )


insertToStash : AccountId -> GroupId -> Password -> SyncData -> SyncData
insertToStash accountId groupId pw sync =
    { sync | passwordStash = Dict.insert accountId ( groupId, pw ) sync.passwordStash }


deletePassword : AccountId -> SyncData -> SyncData
deletePassword key sync =
    -- TODO: also remove my shares + tell others to delete theirs?
    -- Or better, link the datastructures to automatically remove them
    -- TODO: should this even be allowed without unlocking the group??
    -- delete should only be possible by solving a challenge, proving you know the password.
    -- e.g. store hash(hash(pw||rand)) = challenge, rand
    -- to delete solve challenge with hash(pw||rand) = solution, others can check if hash(solution) = challenge
    --
    -- Also, we probably never really want to delete stuff, so it should go into a bin first.
    -- Then a user can empty a bin, but this will only empty the local bin, not all bins.
    --
    -- Also clear cache in the groupPasswordRequestsState and stash here
    updateShared (\s -> { s | passwords = ORDict.remove key s.passwords }) sync


mapAccountsForSite : String -> (GroupId -> AccountId -> Status -> a) -> SyncData -> List a
mapAccountsForSite site f sync =
    List.map
        (\( loginName, groupId ) ->
            f groupId ( site, loginName ) (Request.getStatus groupId sync.groupPasswordRequestsState)
        )
        (getAccountsForSite site sync)


mapGroups : (GroupId -> Status -> Dict String (Dict String PasswordStatus) -> a) -> SyncData -> List a
mapGroups f sync =
    (encryptedPasswords sync)
        |> Dict.foldl
            (\(( siteName, userName ) as accountId) ( groupId, encPw ) acc ->
                let
                    status =
                        Request.getPwStatus accountId groupId sync.groupPasswordRequestsState

                    inner =
                        Dict.singleton userName status
                in
                    Helper.insertOrUpdate groupId
                        (Dict.singleton siteName inner)
                        (Helper.insertOrUpdate siteName inner (Dict.insert userName status))
                        acc
            )
            Dict.empty
        |> Dict.foldl
            (\groupId dict acc ->
                f groupId (Request.getStatus groupId sync.groupPasswordRequestsState) dict :: acc
            )
            []


addShare : Time -> GroupId -> SecretSharing.Share -> SyncData -> ( SyncData, Maybe FillFormData )
addShare time groupId share sync =
    -- TODO: anything else to do here?
    -- Yes: check if some of the tasks inside the stash can be completed
    -- If so, don't forget to insert in storage => Doesn't work yet => INVESTIGATE
    -- TODO: test if this works
    let
        ( newReqState, mayForm ) =
            Request.addShare groupId share sync.groupPasswordRequestsState

        newSync =
            { sync | groupPasswordRequestsState = newReqState }
    in
        case Request.getGroupPassword groupId newReqState of
            Just groupPw ->
                -- insert password from stash into storage
                ( (Dict.foldl
                    (\accountId ( groupId, pw ) accSync ->
                        insertToStorage time groupPw accountId groupId pw accSync
                    )
                    newSync
                    newSync.passwordStash
                  )
                    -- remove pw from stash(es)
                    |> clearStashes groupId
                , mayForm
                )

            Nothing ->
                ( newSync, mayForm )


requestPasswordPressed : GroupId -> Maybe AccountId -> SyncData -> ( SyncData, Maybe FillFormData )
requestPasswordPressed groupId mayAccount sync =
    let
        newReqState =
            Request.waitFor groupId mayAccount (getShare groupId sync) sync.groupPasswordRequestsState

        newSync =
            { sync | groupPasswordRequestsState = newReqState }
    in
        case Request.getStatus groupId newReqState of
            Request.Done (Just ( site, login )) pw ->
                ( newSync, Just { password = pw, site = site, login = login } )

            _ ->
                ( newSync, Nothing )


updateGroupPasswordRequest : (Request.State -> Request.State) -> SyncData -> SyncData
updateGroupPasswordRequest f sync =
    { sync | groupPasswordRequestsState = f sync.groupPasswordRequestsState }


currentGroupId : Int -> SyncData -> String
currentGroupId level sync =
    -- TODO: remove in faviour of giving the user a choice
    groups sync
        |> List.filter (\( l, g ) -> l == level)
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault sync.id


hasPasswordFor : AccountId -> SyncData -> Bool
hasPasswordFor key sync =
    encryptedPasswords sync
        |> Dict.member key


merge : Time -> OtherSharedData -> SyncData -> SyncData
merge timestamp other my =
    let
        newSharesToDistribute =
            ORDict.merge TimestampedVersionRegister.merge
                other.shared.sharesToDistribute
                my.shared.sharesToDistribute

        newPasswords =
            ORDict.merge TimestampedVersionRegister.merge other.shared.passwords my.shared.passwords

        ( myShares, sharesForOthers, didTakeShare ) =
            getMyShares my.id
                my.myShares
                (ORDict.getWith TimestampedVersionRegister.get newSharesToDistribute)
    in
        { my
            | shared =
                { knownIds = ORDict.merge SingleVersionRegister.merge other.shared.knownIds my.shared.knownIds
                , passwords = newPasswords
                , sharesToDistribute =
                    ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp)
                        sharesForOthers
                        newSharesToDistribute
                , version = VClock.merge other.shared.version my.shared.version
                }
            , myShares = myShares
        }
            |> receiveVersion other.id other.shared.version
            |> incrementIf didTakeShare


receiveVersion : String -> VClock -> SyncData -> SyncData
receiveVersion from version sync =
    { sync | synchedWith = Dict.insert from version sync.synchedWith }


{-| take out my shares if there are any
-}
getMyShares :
    String
    -> Dict GroupId SecretSharing.Share
    -> Dict GroupId (Dict String SecretSharing.Share)
    -> ( Dict GroupId SecretSharing.Share, Dict GroupId (Dict String SecretSharing.Share), Bool )
getMyShares id myOldShares sharesToDistribute =
    Dict.foldl
        (\( siteName, userName ) sharesForOthers ( myShares, other, didTakeShare ) ->
            case Dict.get id sharesForOthers of
                Just share ->
                    ( Dict.insert ( siteName, userName ) share myShares
                    , Dict.insert ( siteName, userName ) (Dict.remove id sharesForOthers) other
                    , True
                    )

                Nothing ->
                    ( myShares, other, didTakeShare )
        )
        ( myOldShares, sharesToDistribute, False )
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
    sharedDecoder
        |> JD.map (\shared -> { id = id, shared = shared })


sharedDecoder : Decoder SharedData
sharedDecoder =
    JD.field "dataVersion" JD.int
        |> JD.andThen
            (\v ->
                case v of
                    1 ->
                        JD.field "data" sharedDecoderV1

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


sharedDecoderV1 : Decoder SharedData
sharedDecoderV1 =
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
                (TimestampedVersionRegister.decoder (JD.dict SecretSharing.shareDecoder))
        )
        (JD.field "version" VClock.decoder)


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
    JD.map7
        (\id shared myShares synchedWith passwordStash groupPasswordStash seed ->
            { id = id
            , shared = shared
            , myShares = myShares
            , synchedWith = synchedWith
            , passwordStash = passwordStash
            , groupPasswordStash = groupPasswordStash
            , groupPasswordRequestsState = Request.init
            , seed = seed
            }
        )
        (JD.field "id" JD.string)
        (JD.field "shared" sharedDecoder)
        (JD.field "myShares" <| JD.dict2 groupIdDecoder SecretSharing.shareDecoder)
        (JD.field "synchedWith" <| JD.dict VClock.decoder)
        (JD.field "passwordStash" <| JD.dict2 accountIdDecoder (decodeTuple2 groupIdDecoder JD.string))
        (JD.field "groupPasswordStash" <| JD.dict2 groupIdDecoder JD.string)
        (JD.field "seed" Random.fromJson)


encodeShared : SharedData -> Value
encodeShared shared =
    JE.object
        [ ( "data"
          , JE.object
                [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) shared.knownIds )
                , ( "passwords"
                  , ORDict.encode2 encodeAccountId
                        (TimestampedVersionRegister.encode (encodeTuple2 encodeGroupId encodeEncryptedPassword))
                        shared.passwords
                  )
                , ( "sharesToDistribute"
                  , ORDict.encode2 encodeGroupId
                        (TimestampedVersionRegister.encode (JE.dict identity SecretSharing.encodeShare))
                        shared.sharesToDistribute
                  )
                , ( "version", VClock.encode shared.version )
                ]
          )

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
        , ( "groupPasswordStash", JE.dict (encodeGroupId >> JE.encode 0) JE.string s.groupPasswordStash )
        , ( "seed", Random.toJson s.seed )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 1 )
        ]


encodeVersion : SyncData -> Value
encodeVersion sync =
    VClock.encode sync.shared.version
