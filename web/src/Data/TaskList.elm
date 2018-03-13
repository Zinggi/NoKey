module Data.TaskList exposing (TaskList)

import EverySet as Set exposing (EverySet)
import Dict exposing (Dict)
import Data exposing (..)


{-
   TODO: actually use this in sync data

      This module should be able to handle things like:

          Move Passwords for AccountId from (local) stash into GroupId
              => user needs to unlock GroupId
          Move Passwords for AccountId from GroupId1 into GroupId2
              => user needs to unlock both groups 1 and 2
          Create shares for Ids for GroupIds
              => user needs to unlock GroupIds
-}


type alias TaskList =
    EverySet Task


type
    Task
    -- TODO: add more cases, e.g:
    --      MoveFromGroupToGroup { pws: List AccountId, from: GroupId, to: GroupId }
    = MoveFromStashToGroup { pws : Dict AccountId Password, group : GroupId, groupPw : GroupPassword }


getStash : TaskList -> Dict AccountId Password
getStash tasks =
    Set.foldl
        (\t acc ->
            case t of
                MoveFromStashToGroup { pws } ->
                    Dict.foldl Dict.insert acc pws
         -- _ ->
         --     acc
        )
        Dict.empty
        tasks



-- clearStash : GroupId -> TaskList -> TaskList
-- clearStash groupId tasks =
