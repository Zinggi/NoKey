module Data.TaskList
    exposing
        ( TaskList
        , Task(..)
        , getTasks
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
import Data.RequestGroupPassword as Request exposing (PasswordStatus(..), Status)
import Data exposing (..)
import Helper exposing (encodeTuple3, decodeTuple3)


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
    }


init : TaskList
init =
    { stash = Dict.empty, groupPws = Dict.empty, visiblePws = Set.empty }


encode : TaskList -> Value
encode tasks =
    JE.object
        [ ( "stash", JE.dict (encodeAccountId >> JE.encode 0) (encodeTuple3 encodeGroupId encodeReason JE.string) tasks.stash )
        , ( "groupPws", JE.dict (encodeGroupId >> JE.encode 0) JE.string tasks.groupPws )
        ]


decoder : Decoder TaskList
decoder =
    JD.map2 (\stash groupPws -> { stash = stash, groupPws = groupPws, visiblePws = Set.empty })
        (JD.field "stash" <| JD.dict2 accountIdDecoder (decodeTuple3 groupIdDecoder reasonDecoder JD.string))
        (JD.field "groupPws" <| JD.dict2 groupIdDecoder JD.string)


type
    Task
    -- TODO: add more cases, e.g:
    --      MoveFromGroupToGroup { pws: List AccountId, from: GroupId, to: GroupId }
    = MoveFromStashToGroup { accounts : Dict String (Dict String PasswordStatus), group : GroupId, status : Status }
    | WaitForKeysDistributed { accounts : Dict String (Dict String PasswordStatus), group : GroupId, status : Status, progress : Int }
    | CreateMoreShares { for : List Device, group : GroupId, status : Status }


resolveWaitingTasks : Dict GroupId (Set String) -> TaskList -> TaskList
resolveWaitingTasks dict tasks =
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
        dict


getProgress : GroupId -> Dict GroupId (Set String) -> Int
getProgress groupId dict =
    Dict.get groupId dict |> Maybe.map Set.size |> Maybe.withDefault 0


getTasks : Request.State -> Dict GroupId (List Device) -> Dict GroupId (Set String) -> TaskList -> List Task
getTasks request groupsNotFullyDistributed progress tasks =
    -- Collect all same groups into a single task
    Dict.foldl
        (\(( siteName, userName ) as accountId) ( groupId, reason, pw ) acc ->
            let
                insert d =
                    Dict.insert userName
                        (if Set.member ( siteName, userName ) tasks.visiblePws then
                            Unlocked pw
                         else
                            UnlockedButHidden
                        )
                        d

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
                    [ MoveFromStashToGroup { accounts = moveFromStashToGroup, group = groupId, status = Request.getStatus groupId request } ]
                )
                    ++ (if Dict.isEmpty waitForKeysDistributed then
                            []
                        else
                            [ WaitForKeysDistributed
                                { accounts = waitForKeysDistributed
                                , group = groupId
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
                        CreateMoreShares { group = groupId, status = Request.getStatus groupId request, for = devices } :: acc
                    )
                    ts
                    groupsNotFullyDistributed
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
