module Data.RequestGroupPassword exposing (State, Status(..), init, getStatus, addShare, waitFor, getStatusForSite, getWaiting, removeWaiting)

import Dict exposing (Dict)
import SecretSharing exposing (Share)
import Data.Sync exposing (SyncData, GroupId, AccountId, GroupPassword)
import Helper exposing (maybeToList)


type State
    = State (Dict GroupId Info)


type alias Info =
    { fillForm : Maybe AccountId, shares : List Share, password : Maybe (Result String String) }


type Status
    = NotRequested
    | Waiting Int Int
    | Done (Maybe AccountId) GroupPassword
    | Error String


init : State
init =
    State Dict.empty


getStatus : GroupId -> State -> Status
getStatus key (State state) =
    mayInfoToStatus key (Dict.get key state)


mayInfoToStatus : GroupId -> Maybe Info -> Status
mayInfoToStatus ( level, _ ) mayInfo =
    case mayInfo of
        Nothing ->
            NotRequested

        Just info ->
            case info.password of
                Just (Ok pw) ->
                    Done info.fillForm pw

                Just (Err r) ->
                    Error r

                Nothing ->
                    Waiting (List.length info.shares) level


waitFor : GroupId -> Maybe AccountId -> Maybe Share -> State -> State
waitFor key fillForm maybeMyShare (State state) =
    State <|
        Dict.insert key
            (tryGetPassword key
                { fillForm = fillForm
                , shares = maybeToList maybeMyShare
                , password = Nothing
                }
            )
            state


getWaiting : State -> List GroupId
getWaiting (State state) =
    Dict.filter
        (\_ info -> isWaiting info)
        state
        |> Dict.keys


isWaiting : Info -> Bool
isWaiting info =
    info.password == Nothing


removeWaiting : State -> State
removeWaiting (State state) =
    State <|
        Dict.filter
            (\_ info -> not (isWaiting info))
            state


getStatusForSite : String -> SyncData -> State -> Dict String Status
getStatusForSite site sync (State state) =
    let
        accounts =
            Data.Sync.getAccountsForSite site sync
    in
        List.foldl
            (\( loginName, groupId ) acc ->
                mayInfoToStatus groupId (Dict.get groupId state)
                    |> \info -> Dict.insert loginName info acc
            )
            Dict.empty
            accounts


addShare : GroupId -> Share -> State -> State
addShare key share (State state) =
    (State <|
        Dict.update key
            (Maybe.map (\info -> tryGetPassword key { info | shares = share :: info.shares }))
            state
    )


tryGetPassword : GroupId -> Info -> Info
tryGetPassword ( level, _ ) info =
    case info.password of
        Just pw ->
            info

        Nothing ->
            if (List.length info.shares) >= level then
                -- expensive operation
                { info | password = Just <| SecretSharing.joinToString info.shares }
            else
                info
