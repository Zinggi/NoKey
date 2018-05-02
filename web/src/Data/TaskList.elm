module Data.TaskList
    exposing
        ( TaskList
        , Task(..)
        , getTasks
        , moveAccountFromTo
        , processMoveFromTo
        , getProgress
        , resolveWaitingTasks
        , getStashFor
        , clearStash
        , init
        , getStashPw
        , insertGroupPw
        , insertPwToStash
        , deletePwFromStash
        , togglePassword
        , encode
        , decoder
        , Reason(..)
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline as JD exposing (required, optional)
import Data.RequestGroupPassword as Request exposing (PasswordStatus(..), Status)
import Data exposing (..)
import Helper exposing (encodeTuple3, decodeTuple3, encodeTuple, decodeTuple, encodeSet, decodeSet)


{-

   This module should be able to handle things like:

       Wait until GroupPasswords has been distributed
           => user has to use other devices
       Move Passwords for AccountId from (local) stash into GroupId
           => user needs to unlock GroupId
       Move Passwords for AccountId from GroupId1 into GroupId2
           => user needs to unlock both groups 1 and 2
       Create shares for Ids for GroupIds
           => user needs to unlock GroupIds
-}


type alias TaskList =
    { stash : Dict AccountId ( GroupId, Reason, Password )
    , visiblePws : Set AccountId
    , groupPws : Dict GroupId GroupPassword
    , movePws : Dict ( GroupId, GroupId ) (Set AccountId)
    }


init : TaskList
init =
    { stash = Dict.empty, groupPws = Dict.empty, visiblePws = Set.empty, movePws = Dict.empty }


encode : TaskList -> Value
encode tasks =
    JE.object
        [ ( "stash", JE.dict (encodeAccountId >> JE.encode 0) (encodeTuple3 encodeGroupId encodeReason JE.string) tasks.stash )
        , ( "groupPws", JE.dict (encodeGroupId >> JE.encode 0) (List.map JE.int >> JE.list) tasks.groupPws )
        , ( "movePws", JE.dict (encodeTuple encodeGroupId >> JE.encode 0) (encodeSet encodeAccountId) tasks.movePws )
        ]


decoder : Decoder TaskList
decoder =
    JD.decode (\stash groupPws movePws -> { stash = stash, groupPws = groupPws, visiblePws = Set.empty, movePws = movePws })
        |> required "stash" (JD.dict2 accountIdDecoder (decodeTuple3 groupIdDecoder reasonDecoder JD.string))
        |> required "groupPws" (JD.dict2 groupIdDecoder (JD.list JD.int))
        |> optional "movePws" (JD.dict2 (decodeTuple groupIdDecoder) (decodeSet accountIdDecoder)) Dict.empty


{-| This type contains all relevant information for the UI
-}
type Task
    = MoveFromGroupToGroup { accounts : Dict String (Dict String ()), from : Group, fromStatus : Status, to : Group, toStatus : Status }
    | MoveFromStashToGroup { accounts : Dict String (Dict String ()), group : Group, status : Status }
    | WaitForKeysDistributed { accounts : Dict String (Dict String ()), group : Group, status : Status, progress : Int }
    | CreateMoreShares { for : List Device, group : Group, status : Status }


resolveWaitingTasks : Dict AccountId GroupId -> Dict GroupId (Set String) -> TaskList -> TaskList
resolveWaitingTasks accounts newDistributedShares tasks =
    Dict.foldl
        (\(( level, _ ) as groupId) set acc ->
            if Set.size set >= level then
                { acc
                    | groupPws = Dict.remove groupId acc.groupPws
                    , stash =
                        Dict.filter
                            (\accountId ( gId, reason, pw ) ->
                                not (gId == groupId && reason == KeysNotYetDistributed)
                            )
                            acc.stash
                }
            else
                acc
        )
        tasks
        newDistributedShares
        -- Resolve MoveFromGroupToGroup
        |> (\ts ->
                Dict.foldl
                    (\( from, to ) accountIds acc ->
                        if haveMovedToDestination accounts accountIds to then
                            { acc | movePws = Dict.remove ( from, to ) acc.movePws }
                        else
                            acc
                    )
                    ts
                    ts.movePws
           )


moveAccountFromTo : AccountId -> GroupId -> GroupId -> TaskList -> TaskList
moveAccountFromTo accountId from to tasks =
    { tasks | movePws = Helper.insertOrUpdate ( from, to ) (Set.singleton accountId) (Set.insert accountId) tasks.movePws }


processMoveFromTo : (AccountId -> GroupId -> GroupId -> a -> a) -> TaskList -> a -> a
processMoveFromTo f tasks start =
    Dict.foldl
        (\( from, to ) accounts acc ->
            Set.foldl (\accountId acc2 -> f accountId from to acc2) acc accounts
        )
        start
        tasks.movePws


haveMovedToDestination : Dict AccountId GroupId -> Set AccountId -> GroupId -> Bool
haveMovedToDestination accounts accountIds destination =
    Set.toList accountIds
        |> List.all (\accountId -> Dict.get accountId accounts == Just destination)


getProgress : GroupId -> Dict GroupId (Set String) -> Int
getProgress groupId dict =
    Dict.get groupId dict |> Maybe.map Set.size |> Maybe.withDefault 0


getTasks : Request.State -> Dict GroupId (List Device) -> Dict GroupId (Set String) -> Dict GroupId String -> TaskList -> List Task
getTasks request groupsNotFullyDistributed progress postFixDict tasks =
    let
        getGroup groupId =
            ( groupId, Helper.dictGetWithDefault "" groupId postFixDict )
    in
        -- Collect all same groups into a single task
        Dict.foldl
            (\(( siteName, userName ) as accountId) ( groupId, reason, pw ) acc ->
                let
                    insert d =
                        Dict.insert userName () d

                    insertOrUpdate it =
                        Helper.insertOrUpdate siteName (insert Dict.empty) insert it

                    add it =
                        case reason of
                            GroupLocked ->
                                { it | moveFromStashToGroup = insertOrUpdate it.moveFromStashToGroup }

                            KeysNotYetDistributed ->
                                { it | waitForKeysDistributed = insertOrUpdate it.waitForKeysDistributed }
                in
                    Helper.insertOrUpdate groupId (add { moveFromStashToGroup = Dict.empty, waitForKeysDistributed = Dict.empty }) add acc
            )
            Dict.empty
            tasks.stash
            -- convert to tasks
            |> Dict.foldl
                (\groupId { moveFromStashToGroup, waitForKeysDistributed } acc ->
                    (if Dict.isEmpty moveFromStashToGroup then
                        -- only add if not empty
                        []
                     else
                        [ MoveFromStashToGroup { accounts = moveFromStashToGroup, group = getGroup groupId, status = Request.getStatus groupId request } ]
                    )
                        ++ (if Dict.isEmpty waitForKeysDistributed then
                                []
                            else
                                [ WaitForKeysDistributed
                                    { accounts = waitForKeysDistributed
                                    , group = getGroup groupId
                                    , progress = getProgress groupId progress
                                    , status = Request.getStatus groupId request
                                    }
                                ]
                           )
                        ++ acc
                )
                []
            -- add CreateMoreShares task
            |> (\ts ->
                    Dict.foldl
                        (\groupId devices acc ->
                            CreateMoreShares { group = getGroup groupId, status = Request.getStatus groupId request, for = devices } :: acc
                        )
                        ts
                        groupsNotFullyDistributed
               )
            -- add MoveFromGroupToGroup tasks
            |> (\ts ->
                    Dict.foldl
                        (\( from, to ) accountIds acc ->
                            let
                                insert userName d =
                                    Dict.insert userName () d

                                accounts =
                                    Set.foldl
                                        (\( siteName, login ) acc ->
                                            Helper.insertOrUpdate siteName (insert login Dict.empty) (insert login) acc
                                        )
                                        Dict.empty
                                        accountIds
                            in
                                MoveFromGroupToGroup
                                    { from = getGroup from
                                    , to = getGroup to
                                    , fromStatus = Request.getStatus from request
                                    , toStatus = Request.getStatus to request
                                    , accounts = accounts
                                    }
                                    :: acc
                        )
                        ts
                        tasks.movePws
               )


insertGroupPw : GroupId -> GroupPassword -> TaskList -> TaskList
insertGroupPw groupId pw tasks =
    { tasks | groupPws = Dict.insert groupId pw tasks.groupPws }


type Reason
    = KeysNotYetDistributed
    | GroupLocked


encodeReason : Reason -> Value
encodeReason reason =
    case reason of
        KeysNotYetDistributed ->
            JE.string "KeysNotYetDistributed"

        GroupLocked ->
            JE.string "GroupLocked"


reasonDecoder : Decoder Reason
reasonDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "KeysNotYetDistributed" ->
                        JD.succeed KeysNotYetDistributed

                    "GroupLocked" ->
                        JD.succeed GroupLocked

                    e ->
                        JD.fail (e ++ " is not a valid value for Reason")
            )


insertPwToStash : Reason -> GroupId -> AccountId -> Password -> TaskList -> TaskList
insertPwToStash reason groupId accountId pw tasks =
    { tasks | stash = Dict.insert accountId ( groupId, reason, pw ) tasks.stash }



-- getStash : TaskList -> Dict String (Dict String PasswordStatus)
-- getStash tasks =
--     Dict.map (\_ ( _, _, pw ) -> pw) tasks.stash
--         |> Dict.foldl
--             (\( siteName, userName ) pw acc ->
--                 let
--                     add it =
--                         Dict.insert userName
--                             (if Set.member ( siteName, userName ) tasks.visiblePws then
--                                 Unlocked pw
--                              else
--                                 UnlockedButHidden
--                             )
--                             it
--                 in
--                     Helper.insertOrUpdate siteName (add Dict.empty) add acc
--             )
--             Dict.empty


togglePassword : AccountId -> TaskList -> TaskList
togglePassword accountId tasks =
    if Set.member accountId tasks.visiblePws then
        { tasks | visiblePws = Set.remove accountId tasks.visiblePws }
    else
        { tasks | visiblePws = Set.insert accountId tasks.visiblePws }


getStashPw : AccountId -> TaskList -> Maybe Password
getStashPw accountId tasks =
    Dict.get accountId tasks.stash |> Maybe.map (\( _, _, pw ) -> pw)


getStashFor : GroupId -> TaskList -> Dict AccountId Password
getStashFor groupId tasks =
    Dict.foldl
        (\accountId ( gId, _, pw ) acc ->
            if gId == groupId then
                Dict.insert accountId pw acc
            else
                acc
        )
        Dict.empty
        tasks.stash


deletePwFromStash : AccountId -> TaskList -> TaskList
deletePwFromStash accountId tasks =
    { tasks | stash = Dict.remove accountId tasks.stash }


clearStash : GroupId -> TaskList -> TaskList
clearStash groupId tasks =
    { tasks
        | stash = Dict.filter (\accountId ( gId, _, _ ) -> gId /= groupId) tasks.stash
        , groupPws = Dict.remove groupId tasks.groupPws
    }
