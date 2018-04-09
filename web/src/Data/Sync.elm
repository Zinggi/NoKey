module Data.Sync exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Set exposing (Set)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Json.Decode.Pipeline exposing (decode, required)
import Random.Pcg as Random exposing (Seed)
import Random.Pcg.Extended as RandomE
import Time exposing (Time)
import Murmur3


--

import Helper exposing (decodeTuple2, encodeTuple2)
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
    , encryptionKey : Value
    , signingKey : Value
    , deviceType : DeviceType

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
    { -- TODO: It shouldn't be possible to change the public key under any circumstances to prevent
      -- a byzantine error. E.g. a malicous device could easily change all public keys making communication impossible.
      -- a device could also just delete and then re-add an entry, circumventing any protection.
      -- => the protection would have to be at the ORDict level
      knownIds : ORDict DeviceId (SingleVersionRegister DeviceInfo)

    -- These shares will be taken out one by one by the device whose id matches the id
    -- in the second dict
    -- Here we store encrypted shares with encrypted with the key of the receiving device
    , sharesToDistribute : ORDict GroupId (TimestampedVersionRegister (Dict DeviceId Value))

    -- Store who has a share for which group
    --      This way we can:
    --          + remove a pw from the stash, once enough devices took their share
    --          + give the user a better understanding of how it works:
    --              - E.g. if the user clicks on a group in the passwords view, show which devices have a share
    --          + automatically calculate new shares on unlock group for devices
    --            that have no share yet and aren't in the to distribute list
    , distributedShares : ORDict GroupId (GSet DeviceId)

    -- here we store all passwords, encrypted with the group password
    , passwords : ORDict AccountId (TimestampedVersionRegister ( GroupId, EncryptedPassword ))
    , version : VClock
    }


type alias DeviceInfo =
    { deviceType : DeviceType, name : String, encryptionKey : Value, signingKey : Value }


type DeviceType
    = Browser
    | Android
    | WebExtension


sharesToDistribute : SyncData -> Dict GroupId (Dict DeviceId Value)
sharesToDistribute sync =
    ORDict.getWith TimestampedVersionRegister.get sync.shared.sharesToDistribute


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


{-| given a list of group ids, return their corresponding share.
-}
getShares : Set GroupId -> SyncData -> List ( GroupId, SecretSharing.Share )
getShares groups sync =
    Set.foldl
        (\groupId acc ->
            case Dict.get groupId sync.myShares of
                Just share ->
                    ( groupId, share ) :: acc

                Nothing ->
                    acc
        )
        []
        groups


init : Seed -> Value -> Value -> DeviceType -> String -> SyncData
init seed encryptionKey signingKey devType uuid =
    { shared = initShared seed encryptionKey signingKey devType uuid
    , synchedWith = Dict.empty
    , id = uuid
    , encryptionKey = encryptionKey
    , signingKey = signingKey
    , deviceType = devType
    , seed = seed
    , myShares = Dict.empty
    , tasks = Tasks.init
    , groupPasswordRequestsState = Request.init
    }


initShared : Seed -> Value -> Value -> DeviceType -> String -> SharedData
initShared seed encryptionKey signingKey devType uuid =
    { knownIds =
        ORDict.init seed
            |> ORDict.insert uuid
                (SingleVersionRegister.init
                    { name = ""
                    , deviceType = devType
                    , encryptionKey = encryptionKey
                    , signingKey = signingKey
                    }
                )
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


knownIds : SyncData -> Dict DeviceId DeviceInfo
knownIds sync =
    ORDict.getWith SingleVersionRegister.get sync.shared.knownIds


getSigningKeyOf : DeviceId -> SyncData -> Maybe Value
getSigningKeyOf id sync =
    knownIds sync |> Dict.get id |> Maybe.map .signingKey


{-| returns Dict Id (Name, IdPart)
the IdPart is used to distinguish devices with the same name
-}
knownDevices : SyncData -> Dict DeviceId ( String, String )
knownDevices sync =
    knownIds sync
        |> Dict.toList
        |> Dict.groupBy (Tuple.second >> .name)
        -- Now we have a Dict Name (List (Id, DevInfo))
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


{-| get all groups that have their shares not fully distributed yet.
The value in the Dict is the list of devices that don't have a share yet
-}
groupsNotFullyDistributed : SyncData -> Dict GroupId (List Device)
groupsNotFullyDistributed sync =
    let
        shares : Dict GroupId (Set DeviceId)
        shares =
            distributedShares sync

        distShares : Dict GroupId (Dict DeviceId Value)
        distShares =
            sharesToDistribute sync

        known : Dict DeviceId ( String, String )
        known =
            knownDevices sync

        allDevs : Set DeviceId
        allDevs =
            Dict.foldl (\key _ -> Set.insert key) Set.empty known

        numDevices =
            Dict.size known

        groupsWithShares : Dict GroupId (Set DeviceId)
        groupsWithShares =
            Dict.foldl
                (\groupId devices acc ->
                    let
                        add s =
                            Set.union s (Dict.keys devices |> Set.fromList)
                    in
                        Helper.insertOrUpdate groupId (add Set.empty) add acc
                )
                shares
                distShares
    in
        Dict.foldl
            (\groupId devs acc ->
                if Set.size devs <= numDevices then
                    let
                        others =
                            Set.diff allDevs devs
                    in
                        if Set.isEmpty others then
                            acc
                        else
                            Dict.insert groupId
                                (Set.foldl
                                    (\devId acc2 ->
                                        let
                                            ( name, post ) =
                                                Dict.get devId known |> Maybe.withDefault ( "", "" )
                                        in
                                            { id = devId, name = name, postFix = post } :: acc2
                                    )
                                    []
                                    others
                                )
                                acc
                else
                    acc
            )
            Dict.empty
            groupsWithShares


devicesNeedingSharesFor : GroupId -> SyncData -> List DeviceId
devicesNeedingSharesFor groupId sync =
    case Dict.get groupId (groupsNotFullyDistributed sync) of
        Just devs ->
            List.map .id devs

        Nothing ->
            []


displayNamesKnownDevices : SyncData -> List Device
displayNamesKnownDevices sync =
    knownDevices sync
        |> Dict.foldl (\name ( id, part ) acc -> { id = id, name = name, postFix = part } :: acc) []


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
    init sync.seed sync.encryptionKey sync.signingKey sync.deviceType sync.id
        -- keep version history of this device
        |> updateShared (\s -> { s | knownIds = ORDict.resetExceptOne sync.id sync.shared.knownIds })


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    updateShared
        (\s ->
            { s
                | knownIds =
                    ORDict.update sync.id
                        (SingleVersionRegister.update (\v -> { v | name = newName }))
                        s.knownIds
            }
        )
        sync


getPassword : AccountId -> SyncData -> Maybe Password
getPassword accountId sync =
    Request.getPassword accountId
        (Dict.get accountId (encryptedPasswords sync))
        (Tasks.getStashPw accountId sync.tasks)
        sync.groupPasswordRequestsState


getXValues : SyncData -> Dict DeviceId Int
getXValues sync =
    -- TODO: what do in case of a collision???
    knownDevices sync
        {- 42 might seem like a random value, but it has to be 42!
           The reason is that Zinggi/elm-hash-icon uses the same hash function with a seed of 42 as well.
           This way, a collision here will also show in the icon (the reverse is not true):
           If there is a collision here, you will also have two icons that are the same.
           My theory is that users don't want to have the same icons, so they reset one device to get a new icon
           which in turn also resolves the collision here!
        -}
        |> Dict.map (\id _ -> Murmur3.hashString 42 id)


getXValuesFor : List DeviceId -> SyncData -> Dict DeviceId Int
getXValuesFor devs _ =
    -- TODO: the sync parameter is ignored, because we just use the hash until we have an idea how
    -- to deal with hash collisions
    devs
        |> List.foldl
            (\id acc ->
                Dict.insert id (Murmur3.hashString 42 id) acc
            )
            Dict.empty


type alias ShouldAddNewShares msg =
    Time -> GroupId -> List ( DeviceId, ( Value, Value ) ) -> Cmd msg


{-| The caller is expected to call Api.requestShare if the third part of the tuple is True.
-}
insertSite :
    ShouldAddNewShares msg
    -> Time
    -> RandomE.Seed
    -> AccountId
    -> GroupId
    -> Password
    -> SyncData
    -> ( SyncData, RandomE.Seed, Bool, Cmd msg )
insertSite onShouldAddNewShares time seed accountId groupId pw sync =
    case Request.getGroupPassword groupId sync.groupPasswordRequestsState of
        -- if have group password then
        Just groupPw ->
            -- insert into passwords
            ( insertToStorage time groupPw accountId groupId pw sync, seed, False, Cmd.none )

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
                    , Cmd.none
                    )
                else
                    let
                        -- generate group password
                        ( groupPw, seed2 ) =
                            RandomE.step Helper.groupPwGenerator seed

                        -- generate shares/keys
                        ( shares, seed3 ) =
                            SecretSharing.splitBytes ( level, getXValues sync ) groupPw seed2

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
                                |> updateShared (\s -> { s | distributedShares = newDistributedShares })
                                -- insert groupPw to stash (since the keys are not yet distributed)
                                |> (\s -> { s | tasks = Tasks.insertGroupPw groupId groupPw s.tasks })
                                -- store pw in passwords
                                |> insertToStorage time groupPw accountId groupId pw
                                -- add both groupPw and pw to cache (TODO: should I really do this?
                                --   might be easier to just pass the stash along when needed by Request. module)
                                |> updateGroupPasswordRequest (Request.cacheAccountPw accountId pw False >> Request.cacheGroupPw groupId groupPw)
                    in
                        ( newSync, seed3, False, onShouldAddNewShares time groupId (getAssociatedKeys sharesForOthers newSync) )


getEncryptionKeyOf : DeviceId -> SyncData -> Maybe Value
getEncryptionKeyOf devId sync =
    knownIds sync
        |> Dict.get devId
        |> Maybe.map .encryptionKey


getAssociatedKeys : Dict DeviceId SecretSharing.Share -> SyncData -> List ( DeviceId, ( Value, Value ) )
getAssociatedKeys dict sync =
    let
        known =
            knownIds sync

        shares =
            Dict.foldl
                (\devId v acc ->
                    case Dict.get devId known of
                        Just info ->
                            Dict.insert devId ( info.encryptionKey, v ) acc

                        Nothing ->
                            acc
                )
                Dict.empty
                dict
    in
        shares
            |> Dict.map (\key ( key, share ) -> ( key, SecretSharing.encodeShare share ))
            |> Dict.toList


addNewShares : Time -> GroupId -> List ( DeviceId, Value ) -> SyncData -> SyncData
addNewShares time groupId shares sync =
    let
        newShares =
            Dict.fromList shares
    in
        updateShared
            (\s ->
                { s
                    | sharesToDistribute =
                        ORDict.updateOrInsert groupId
                            (TimestampedVersionRegister.update sync.id time (Dict.union newShares))
                            (TimestampedVersionRegister.init sync.id time newShares)
                            s.sharesToDistribute
                }
            )
            sync


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
                Debug.log
                    "Encrypting a password failed? But why???\n(err, groupPw, accountId, groupId, pw)"
                    ( str, groupPw, accountId, groupId, pw )
                    |> always sync


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


getDevice : DeviceId -> SyncData -> Device
getDevice id sync =
    Dict.get id (knownDevices sync)
        |> Maybe.map (\( name, post ) -> { id = id, name = name, postFix = post })
        |> Maybe.withDefault { id = id, name = "", postFix = "" }


mapGroups : (GroupId -> Int -> Status -> Dict String (Dict String PasswordStatus) -> a) -> SyncData -> List a
mapGroups f sync =
    encryptedPasswords sync
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
                f groupId
                    (Tasks.getProgress groupId (distributedShares sync))
                    (Request.getStatus groupId sync.groupPasswordRequestsState)
                    dict
                    :: acc
            )
            []


addShares : (Time -> GroupId -> List ( DeviceId, ( Value, Value ) ) -> Cmd msg) -> Time -> List ( GroupId, SecretSharing.Share ) -> SyncData -> ( SyncData, Maybe AccountId, Cmd msg )
addShares onShouldAddNewShares time shares sync =
    List.foldl
        (\( groupId, share ) ( newSync, mayAcc, cmd ) ->
            let
                ( s, mA, c ) =
                    addShare onShouldAddNewShares time groupId share newSync
            in
                ( s, Maybe.withDefault mayAcc (Maybe.map Just mA), Cmd.batch [ c, cmd ] )
        )
        ( sync, Nothing, Cmd.none )
        shares


addShare : (Time -> GroupId -> List ( DeviceId, ( Value, Value ) ) -> Cmd msg) -> Time -> GroupId -> SecretSharing.Share -> SyncData -> ( SyncData, Maybe AccountId, Cmd msg )
addShare onShouldAddNewShares time groupId share sync =
    -- Add a share. Ff we can unlock a group, check which tasks can be completed, e.g.
    -- moving passwords from stash to storage and generating more shares for those that need them.
    let
        ( newReqState, mayForm ) =
            Request.addShare groupId share sync.groupPasswordRequestsState

        newSync =
            { sync | groupPasswordRequestsState = newReqState }
    in
        case Request.getGroupPassword groupId newReqState of
            Just groupPw ->
                let
                    sync2 =
                        -- insert password from stash into storage
                        Dict.foldl
                            (\accountId pw accSync ->
                                insertToStorage time groupPw accountId groupId pw accSync
                            )
                            newSync
                            (Tasks.getStashFor groupId newSync.tasks)
                            -- remove pw from stash(es)
                            |> clearStashes groupId

                    newShares =
                        -- Generate new shares for those that need them
                        SecretSharing.createMoreShares
                            (getXValuesFor (devicesNeedingSharesFor groupId sync2) sync2)
                            (Request.getAllShares groupId newReqState)

                    cmd =
                        case newShares of
                            Ok shares ->
                                -- Call encryptShares port here
                                onShouldAddNewShares time groupId (getAssociatedKeys shares sync2)

                            Err e ->
                                Debug.log "Failed to create more shares" e
                                    |> always Cmd.none
                in
                    ( sync2, mayForm, cmd )

            Nothing ->
                ( newSync, mayForm, Cmd.none )


{-| Update the group status, and in case we want to fill the password once it's ready, keep track of that
-}
requestPasswordPressed : List GroupId -> Maybe AccountId -> SyncData -> ( SyncData, Maybe FillFormData )
requestPasswordPressed groupIds mayAccount sync =
    let
        newReqState =
            List.foldl (\groupId -> Request.waitFor groupId mayAccount (getShare groupId sync)) sync.groupPasswordRequestsState groupIds

        newSync =
            { sync | groupPasswordRequestsState = newReqState }

        mayFill =
            Request.canFill mayAccount newReqState
                |> Maybe.andThen
                    (\( ( site, login ) as accountId, _ ) ->
                        getPassword accountId newSync
                            |> Maybe.map (\pw -> { password = pw, site = site, login = login })
                    )
    in
        ( newSync, mayFill )


getTasks : SyncData -> List Task
getTasks sync =
    Tasks.getTasks sync.groupPasswordRequestsState (groupsNotFullyDistributed sync) (distributedShares sync) sync.tasks


distributedShares : SyncData -> Dict GroupId (Set DeviceId)
distributedShares sync =
    ORDict.getWith GSet.get sync.shared.distributedShares


updateGroupPasswordRequest : (Request.State -> Request.State) -> SyncData -> SyncData
updateGroupPasswordRequest f sync =
    { sync | groupPasswordRequestsState = f sync.groupPasswordRequestsState }


groupIdsWithShare : List GroupId -> SyncData -> List GroupId
groupIdsWithShare ids sync =
    List.foldl
        (\groupId acc ->
            if Dict.member groupId sync.myShares then
                groupId :: acc
            else
                acc
        )
        []
        ids


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


merge : (Dict GroupId Value -> Cmd msg) -> Time -> OtherSharedData -> SyncData -> ( SyncData, Cmd msg )
merge onShouldDecryptMyShares timestamp other my =
    let
        newSharesToDistribute =
            ORDict.merge TimestampedVersionRegister.merge
                other.shared.sharesToDistribute
                my.shared.sharesToDistribute

        mergedDistributedShares =
            ORDict.merge GSet.merge other.shared.distributedShares my.shared.distributedShares

        newPasswords =
            ORDict.merge TimestampedVersionRegister.merge other.shared.passwords my.shared.passwords

        ( myEncryptedShares, sharesForOthers, sharesITook ) =
            getMyShares my.id (ORDict.getWith TimestampedVersionRegister.get newSharesToDistribute)

        newDistributedShares =
            Set.foldl
                (addIdToDistributedShares my.id)
                mergedDistributedShares
                sharesITook

        cmd =
            if Dict.isEmpty myEncryptedShares then
                Cmd.none
            else
                onShouldDecryptMyShares myEncryptedShares
    in
        ( { my
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
          }
            |> receiveVersion other.id other.shared.version
            -- if enough shares/keys are distributed, resolve tasks (remove groupPw + pws from stash).
            |> (\s -> { s | tasks = Tasks.resolveWaitingTasks (ORDict.getWith GSet.get newDistributedShares) s.tasks })
            |> incrementIf (not (Set.isEmpty sharesITook))
        , cmd
        )


addToMyShares : List ( GroupId, Value ) -> SyncData -> SyncData
addToMyShares newShares sync =
    let
        newS =
            List.foldl
                (\( groupId, share ) acc ->
                    case JD.decodeValue SecretSharing.shareDecoder share of
                        Ok s ->
                            Dict.insert groupId s acc

                        Err e ->
                            Debug.log "Can't decode my decrypted share" e
                                |> always acc
                )
                Dict.empty
                newShares
    in
        { sync | myShares = Dict.union newS sync.myShares }


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
    -> Dict GroupId (Dict DeviceId Value)
    -> ( Dict GroupId Value, Dict GroupId (Dict String Value), Set GroupId )
getMyShares id sharesToDistribute =
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
        ( Dict.empty, sharesToDistribute, Set.empty )
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


encodeShared : SharedData -> Value
encodeShared shared =
    JE.object
        [ ( "data"
          , JE.object
                [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode encodeDeviceInfo) shared.knownIds )
                , ( "passwords"
                  , ORDict.encode2 encodeAccountId
                        (TimestampedVersionRegister.encode (encodeTuple2 encodeGroupId encodeEncryptedPassword))
                        shared.passwords
                  )
                , ( "sharesToDistribute"
                  , ORDict.encode2 encodeGroupId
                        (TimestampedVersionRegister.encode (JE.dict identity identity))
                        shared.sharesToDistribute
                  )
                , ( "distributedShares", ORDict.encode2 encodeGroupId GSet.encode shared.distributedShares )
                , ( "version", VClock.encode shared.version )
                ]
          )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 1 )
        ]


sharedDecoderV1 : Decoder SharedData
sharedDecoderV1 =
    JD.map5
        (\knownIds passwords sharesToDistribute distributedShares version ->
            { knownIds = knownIds
            , passwords = passwords
            , sharesToDistribute = sharesToDistribute
            , distributedShares = distributedShares
            , version = version
            }
        )
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder decodeDeviceInfo))
        (JD.field "passwords" <|
            ORDict.decoder2 accountIdDecoder
                (TimestampedVersionRegister.decoder (decodeTuple2 groupIdDecoder encryptedPasswordDecoder))
        )
        (JD.field "sharesToDistribute" <|
            ORDict.decoder2 groupIdDecoder
                (TimestampedVersionRegister.decoder (JD.dict JD.value))
        )
        (JD.field "distributedShares" <| ORDict.decoder2 groupIdDecoder GSet.decoder)
        (JD.field "version" VClock.decoder)


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


encodeComplete : SyncData -> Value
encodeComplete s =
    JE.object
        [ ( "data"
          , JE.object
                [ ( "id", JE.string s.id )
                , ( "encryptionKey", s.encryptionKey )
                , ( "signingKey", s.signingKey )
                , ( "deviceType", encodeDeviceType s.deviceType )
                , ( "shared", encodeShared s.shared )
                , ( "myShares", JE.dict (encodeGroupId >> JE.encode 0) SecretSharing.encodeShare s.myShares )
                , ( "synchedWith", JE.dict identity VClock.encode s.synchedWith )
                , ( "tasks", Tasks.encode s.tasks )
                , ( "seed", Random.toJson s.seed )
                ]
          )

        -- TODO: change if data format changes
        , ( "dataVersion", JE.int 1 )
        ]


appVersion : String
appVersion =
    -- TODO: change if dataVersion above changes
    "0.0.1"


completeDecoder : Decoder SyncData
completeDecoder =
    JD.field "dataVersion" JD.int
        |> JD.andThen
            (\v ->
                case v of
                    1 ->
                        JD.field "data" completeDecoderV1

                    _ ->
                        JD.fail ("cannot decode version " ++ toString v)
            )


completeDecoderV1 : Decoder SyncData
completeDecoderV1 =
    decode
        (\id encryptionKey signingKey deviceType shared myShares synchedWith tasks seed ->
            { id = id
            , encryptionKey = encryptionKey
            , signingKey = signingKey
            , deviceType = deviceType
            , shared = shared
            , myShares = myShares
            , synchedWith = synchedWith
            , tasks = tasks
            , groupPasswordRequestsState = Request.init
            , seed = seed
            }
        )
        |> required "id" JD.string
        |> required "encryptionKey" JD.value
        |> required "signingKey" JD.value
        |> required "deviceType" deviceTypeDecoder
        |> required "shared" sharedDecoder
        |> required "myShares" (JD.dict2 groupIdDecoder SecretSharing.shareDecoder)
        |> required "synchedWith" (JD.dict VClock.decoder)
        |> required "tasks" Tasks.decoder
        |> required "seed" Random.fromJson


encodeDeviceInfo : DeviceInfo -> Value
encodeDeviceInfo info =
    JE.object
        [ ( "name", JE.string info.name )
        , ( "type", encodeDeviceType info.deviceType )
        , ( "encryptionKey", info.encryptionKey )
        , ( "signingKey", info.signingKey )
        ]


decodeDeviceInfo : Decoder DeviceInfo
decodeDeviceInfo =
    JD.map4
        (\name type_ encryptionKey signingKey ->
            { name = name, deviceType = type_, encryptionKey = encryptionKey, signingKey = signingKey }
        )
        (JD.field "name" JD.string)
        (JD.field "type" deviceTypeDecoder)
        (JD.field "encryptionKey" JD.value)
        (JD.field "signingKey" JD.value)


encodeDeviceType : DeviceType -> Value
encodeDeviceType t =
    JE.string (toString t)


deviceTypeDecoder : Decoder DeviceType
deviceTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "WebExtension" ->
                        JD.succeed WebExtension

                    "Browser" ->
                        JD.succeed Browser

                    "Android" ->
                        JD.succeed Android

                    e ->
                        JD.fail (e ++ " isn't a valid instance of DeviceType")
            )


encodeVersion : SyncData -> Value
encodeVersion sync =
    VClock.encode sync.shared.version
