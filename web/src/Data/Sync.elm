module Data.Sync exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Set exposing (Set)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Random.Pcg as Random exposing (Seed)
import Random.Pcg.Extended as RandomE
import Time exposing (Time)


--

import Helper exposing (decodeTuple, decodeTuple2, encodeTuple, encodeTuple2, encodeSet, decodeSet)
import SecretSharing
import AES
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import Crdt.VClock as VClock exposing (VClock)
import Crdt.GSet as GSet exposing (GSet)
import Data.RequestGroupPassword as Request exposing (Status, PasswordStatus)
import Data.TaskList as Tasks exposing (TaskList, Task)
import Data exposing (..)


{-| This represents the data that is shared + all the metadata we need to sync this to others + our own private shares
-}
type alias SyncData =
    { shared : SharedData

    -- private data
    , id : String
    , synchedWith : Dict String VClock
    , myShares : Dict GroupId SecretSharing.Share
    , tasks : TaskList
    , seed : Seed

    -- ephemeral
    , groupPasswordRequestsState : Request.State
    }


clearStashes : GroupId -> SyncData -> SyncData
clearStashes groupId sync =
    { sync | tasks = Tasks.clearStash groupId sync.tasks }


{-| the data we receive on a sync update
-}
type alias OtherSharedData =
    { id : String, shared : SharedData }


{-| the data we want to share across all known devices
-}
type alias SharedData =
    { -- TODO: add device type, e.g. android, browser, extension, ... + public key
      knownIds : ORDict String (SingleVersionRegister String)

    -- These shares will be taken out one by one by the device whose id matches the id
    -- in the second dict
    -- TODO: here we should store encrypted shares with e.g. RSA
    , sharesToDistribute : ORDict GroupId (TimestampedVersionRegister (Dict String SecretSharing.Share))

    -- Store who has a share for which group
    --      This way we can:
    --          + TODO: remove a pw from the stash, once enough devices took their share
    --          + give the user a better understanding of how it works:
    --              - E.g. if the user clicks on a group in the passwords view, show which devices have a share
    --          + TODO: automatically calculate new shares on unlock group for devices
    --            that have no share yet and aren't in the to distribute list
    , distributedShares : ORDict GroupId (GSet DeviceId)

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
        |> (\s -> Dict.foldl (\groupId _ acc -> Set.insert groupId acc) s (ORDict.get sync.shared.distributedShares))
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
    , tasks = Tasks.init
    , groupPasswordRequestsState = Request.init
    }


initShared : Seed -> String -> SharedData
initShared seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init "")
    , sharesToDistribute = ORDict.init seed
    , distributedShares = ORDict.init seed
    , passwords = ORDict.init seed
    , version = VClock.init
    }


togglePassword : AccountId -> SyncData -> SyncData
togglePassword accountId sync =
    { sync
        | groupPasswordRequestsState =
            Request.togglePassword accountId
                (Dict.get accountId (encryptedPasswords sync))
                (Tasks.getStashPw accountId sync.tasks)
                sync.groupPasswordRequestsState
        , tasks = Tasks.togglePassword accountId sync.tasks
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


getPassword : AccountId -> SyncData -> Maybe Password
getPassword accountId sync =
    Request.getPassword accountId
        (Dict.get accountId (encryptedPasswords sync))
        (Tasks.getStashPw accountId sync.tasks)
        sync.groupPasswordRequestsState


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
                addToStash reason =
                    { sync | tasks = Tasks.insertPwToStash reason groupId accountId pw sync.tasks }

                ( level, _ ) =
                    groupId
            in
                -- if groupId already exists then
                if List.member groupId (groups sync) then
                    -- ask others for their shares/keys
                    ( updateGroupPasswordRequest (Request.waitFor groupId Nothing (getShare groupId sync))
                        -- add password to stash (since the group is locked)
                        (addToStash Tasks.GroupLocked)
                    , seed
                    , True
                    )
                else
                    let
                        -- generate group password
                        ( groupPw, seed2 ) =
                            RandomE.step Helper.groupPwGenerator seed

                        -- generate shares/keys
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
                        ( myShares, sharesForOthers, newDistributedShares ) =
                            case Dict.get sync.id shares of
                                Just share ->
                                    ( Dict.insert groupId share sync.myShares
                                    , Dict.remove sync.id shares
                                    , addIdToDistributedShares sync.id groupId sync.shared.distributedShares
                                    )

                                Nothing ->
                                    Debug.log "This should never happen, but there is a save default, so we don't crash"
                                        ( sync.myShares, shares, sync.shared.distributedShares )

                        newSync =
                            -- add password to stash (since keys aren't distributed yet)
                            addToStash Tasks.KeysNotYetDistributed
                                |> (\s -> { s | myShares = myShares })
                                -- TODO: here we should encrypt the shares for the others
                                -- share with others
                                |> updateShared
                                    (\s ->
                                        { s
                                            | sharesToDistribute =
                                                ORDict.updateOrInsert groupId
                                                    (TimestampedVersionRegister.set sync.id time sharesForOthers)
                                                    (TimestampedVersionRegister.init sync.id time sharesForOthers)
                                                    s.sharesToDistribute
                                            , distributedShares = newDistributedShares
                                        }
                                    )
                                -- insert groupPw to stash (since the keys are not yet distributed)
                                |> (\s -> { s | tasks = Tasks.insertGroupPw groupId groupPw s.tasks })
                                -- store pw in passwords
                                |> insertToStorage time groupPw accountId groupId pw
                                -- add both groupPw and pw to cache (TODO: should I really do this?
                                --   might be easier to just pass the stash along when needed by Request. module)
                                |> updateGroupPasswordRequest (Request.cacheAccountPw accountId pw False >> Request.cacheGroupPw groupId groupPw)
                    in
                        ( newSync, seed3, False )


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


deletePassword : AccountId -> SyncData -> SyncData
deletePassword key sync =
    -- TODO: should this even be allowed without unlocking the group??
    -- delete should only be possible by solving a challenge, proving you know the password.
    -- e.g. store hash(hash(pw||rand)) = challenge, rand
    -- to delete solve challenge with hash(pw||rand) = solution, others can check if hash(solution) = challenge
    --
    -- Also, we probably never really want to delete stuff, so it should go into a bin first.
    -- Then a user can empty a bin, but this will only empty the local bin, not all bins.
    --
    -- Also clear cache in the groupPasswordRequestsState
    updateShared (\s -> { s | passwords = ORDict.remove key s.passwords }) sync
        |> (\s -> { s | tasks = Tasks.deletePwFromStash key sync.tasks })


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


addShare : Time -> GroupId -> SecretSharing.Share -> SyncData -> ( SyncData, Maybe AccountId )
addShare time groupId share sync =
    -- TODO: anything else to do here?
    -- Yes: check if some of the tasks inside the stash can be completed
    -- TODO: generate new shares here if we just unlocked a group and there are devices that aren't in
    -- distributedShares and not in sharesToDistribute
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
                    (\accountId pw accSync ->
                        insertToStorage time groupPw accountId groupId pw accSync
                    )
                    newSync
                    (Tasks.getStashFor groupId newSync.tasks)
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


getTasks : SyncData -> List Task
getTasks sync =
    Tasks.getTasks sync.groupPasswordRequestsState (ORDict.getWith GSet.get sync.shared.distributedShares) sync.tasks


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
    -- TODO: if enough shares/keys are distributed, resolve tasks (remove groupPw + pws from stash (as they are already stored)).
    let
        newSharesToDistribute =
            ORDict.merge TimestampedVersionRegister.merge
                other.shared.sharesToDistribute
                my.shared.sharesToDistribute

        mergedDistributedShares =
            ORDict.merge GSet.merge other.shared.distributedShares my.shared.distributedShares

        newPasswords =
            ORDict.merge TimestampedVersionRegister.merge other.shared.passwords my.shared.passwords

        ( myShares, sharesForOthers, sharesITook ) =
            getMyShares my.id
                my.myShares
                (ORDict.getWith TimestampedVersionRegister.get newSharesToDistribute)

        newDistributedShares =
            Set.foldl
                (addIdToDistributedShares my.id)
                mergedDistributedShares
                sharesITook
    in
        { my
            | shared =
                { knownIds = ORDict.merge SingleVersionRegister.merge other.shared.knownIds my.shared.knownIds
                , passwords = newPasswords
                , distributedShares = newDistributedShares
                , sharesToDistribute =
                    ORDict.updateWithDict (TimestampedVersionRegister.set my.id timestamp)
                        sharesForOthers
                        newSharesToDistribute
                , version = VClock.merge other.shared.version my.shared.version
                }
            , myShares = myShares
        }
            |> receiveVersion other.id other.shared.version
            |> (\s -> { s | tasks = Tasks.resolveWaitingTasks (ORDict.getWith GSet.get newDistributedShares) s.tasks })
            |> incrementIf (not (Set.isEmpty sharesITook))


addIdToDistributedShares : DeviceId -> GroupId -> ORDict GroupId (GSet DeviceId) -> ORDict GroupId (GSet DeviceId)
addIdToDistributedShares devId groupId dict =
    ORDict.updateOrInsert groupId (GSet.add devId) (GSet.add devId GSet.init) dict


receiveVersion : String -> VClock -> SyncData -> SyncData
receiveVersion from version sync =
    { sync | synchedWith = Dict.insert from version sync.synchedWith }


{-| take out my shares if there are any
-}
getMyShares :
    String
    -> Dict GroupId SecretSharing.Share
    -> Dict GroupId (Dict String SecretSharing.Share)
    -> ( Dict GroupId SecretSharing.Share, Dict GroupId (Dict String SecretSharing.Share), Set GroupId )
getMyShares id myOldShares sharesToDistribute =
    Dict.foldl
        (\groupId sharesForOthers ( myShares, other, sharesITook ) ->
            case Dict.get id sharesForOthers of
                Just share ->
                    ( Dict.insert groupId share myShares
                    , Dict.insert groupId (Dict.remove id sharesForOthers) other
                    , Set.insert groupId sharesITook
                    )

                Nothing ->
                    ( myShares, other, sharesITook )
        )
        ( myOldShares, sharesToDistribute, Set.empty )
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

                    2 ->
                        JD.field "data" sharedDecoderV2

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


sharedDecoderV2 : Decoder SharedData
sharedDecoderV2 =
    JD.map5
        (\knownIds passwords sharesToDistribute distributedShares version ->
            { knownIds = knownIds
            , passwords = passwords
            , sharesToDistribute = sharesToDistribute
            , distributedShares = distributedShares
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
        (JD.field "distributedShares" <| ORDict.decoder2 groupIdDecoder GSet.decoder)
        (JD.field "version" VClock.decoder)


sharedDecoderV1 : Decoder SharedData
sharedDecoderV1 =
    JD.map4
        (\knownIds passwords sharesToDistribute version ->
            { knownIds = knownIds
            , passwords = passwords
            , sharesToDistribute = sharesToDistribute
            , distributedShares = ORDict.init (Random.initialSeed 0)
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


completeDecoder : Decoder SyncData
completeDecoder =
    JD.field "dataVersion" JD.int
        |> JD.andThen
            (\v ->
                case v of
                    1 ->
                        completeDecoderV1

                    2 ->
                        JD.field "data" completeDecoderV2

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


completeDecoderV2 : Decoder SyncData
completeDecoderV2 =
    JD.map6
        (\id shared myShares synchedWith tasks seed ->
            { id = id
            , shared = shared
            , myShares = myShares
            , synchedWith = synchedWith
            , tasks = tasks
            , groupPasswordRequestsState = Request.init
            , seed = seed
            }
        )
        (JD.field "id" JD.string)
        (JD.field "shared" sharedDecoder)
        (JD.field "myShares" <| JD.dict2 groupIdDecoder SecretSharing.shareDecoder)
        (JD.field "synchedWith" <| JD.dict VClock.decoder)
        (JD.field "tasks" Tasks.decoder)
        (JD.field "seed" Random.fromJson)


completeDecoderV1 : Decoder SyncData
completeDecoderV1 =
    JD.map7
        (\id shared myShares synchedWith passwordStash groupPasswordStash seed ->
            { id = id
            , shared = shared
            , myShares = myShares
            , synchedWith = synchedWith

            -- We should actually read passwordStash and groupPasswordStash and convert it to Tasks, but
            -- Since I was the only user at the time of v1, I didn't bother. (My stash was empty anyway)
            , tasks = Tasks.init
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
                , ( "distributedShares", ORDict.encode2 encodeGroupId GSet.encode shared.distributedShares )
                , ( "version", VClock.encode shared.version )
                ]
          )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 2 )
        ]


encodeComplete : SyncData -> Value
encodeComplete s =
    JE.object
        [ ( "data"
          , JE.object
                [ ( "id", JE.string s.id )
                , ( "shared", encodeShared s.shared )
                , ( "myShares", JE.dict (encodeGroupId >> JE.encode 0) (SecretSharing.encodeShare) s.myShares )
                , ( "synchedWith", JE.dict identity VClock.encode s.synchedWith )
                , ( "tasks", Tasks.encode s.tasks )
                , ( "seed", Random.toJson s.seed )
                ]
          )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 2 )
        ]


encodeVersion : SyncData -> Value
encodeVersion sync =
    VClock.encode sync.shared.version
