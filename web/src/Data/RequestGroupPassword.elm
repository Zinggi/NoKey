module Data.RequestGroupPassword
    exposing
        ( State
        , Status(..)
        , PasswordStatus(..)
        , init
        , getStatus
        , getPwStatus
        , isGroupUnlocked
        , isUnlocked
        , togglePassword
        , getPassword
        , addShare
        , canFill
        , waitFor
        , getWaiting
        , removeWaiting
        , removeWaitingGroups
        , cacheAccountPw
        , invalidatePwCache
        , invalidatePwCaches
        , invalidatePwCacheIfExists
        , getAllShares
        , cacheGroupPw
        , getGroupPassword
        , lockGroups
        , statusToComparable
        , unlockIfPossible
        )

import Dict exposing (Dict)
import Dict.Extra as Dict
import EverySet as Set exposing (EverySet)
import Set as SetStd
import AES
import SecretSharing exposing (Share)
import Data exposing (GroupId, AccountId, GroupPassword, Password, EncryptedPassword)
import Data.KeyBox as KeyBox exposing (KeyBoxes)
import Helper exposing (maybeToEverySet)


type State
    = State { groupPws : Dict GroupId Info, pws : Dict AccountId ( Password, Bool ) }


type alias Info =
    { fillForm : Maybe AccountId, shares : EverySet Share, password : Maybe (Result String GroupPassword) }


type Status
    = NotRequested
    | Waiting Int Int
    | Done (Maybe AccountId) GroupPassword
    | Error String


statusToComparable : Status -> String
statusToComparable st =
    case st of
        Done _ _ ->
            "Done"

        other ->
            toString other


init : State
init =
    State { groupPws = Dict.empty, pws = Dict.empty }


getAllShares : GroupId -> List Share -> State -> List Share
getAllShares groupId myShares (State state) =
    Dict.get groupId state.groupPws
        |> Maybe.map .shares
        |> Maybe.map (\s -> List.foldl (\share acc -> Set.insert share acc) s myShares)
        |> Maybe.map Set.toList
        |> Maybe.withDefault []


getGroupPassword : GroupId -> List Share -> State -> Maybe GroupPassword
getGroupPassword groupId shares state =
    case getStatus groupId shares state of
        Done _ pw ->
            Just pw

        _ ->
            Nothing


isGroupUnlocked : GroupId -> List Share -> State -> Bool
isGroupUnlocked groupId shares state =
    case getStatus groupId shares state of
        Done _ _ ->
            True

        _ ->
            False


{-| forget about all shares we already collected
-}
lockGroups : Dict GroupId (List AccountId) -> State -> State
lockGroups groupIds (State state) =
    State
        { state
            | groupPws = Dict.removeMany (Dict.keys groupIds |> SetStd.fromList) state.groupPws
            , pws = Dict.removeMany (Dict.values groupIds |> List.concat |> SetStd.fromList) state.pws
        }


getStatus : GroupId -> List Share -> State -> Status
getStatus key shares (State state) =
    mayInfoToStatus key shares (Dict.get key state.groupPws)


canFill : (GroupId -> List Share) -> Maybe AccountId -> State -> Maybe ( AccountId, GroupPassword )
canFill getShares mayId (State state) =
    Maybe.andThen
        (\id ->
            Dict.filter (\key info -> info.fillForm == Just id) state.groupPws
                |> Dict.toList
                |> List.head
                |> Maybe.andThen
                    (\( groupId, info ) ->
                        case mayInfoToStatus groupId (getShares groupId) (Just info) of
                            Done (Just accountId) pw ->
                                Just ( accountId, pw )

                            _ ->
                                Nothing
                    )
        )
        mayId


mayInfoToStatus : GroupId -> List Share -> Maybe Info -> Status
mayInfoToStatus ( level, _ ) shares mayInfo =
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
                    Waiting (Set.size info.shares + List.length shares) level


waitFor : GroupId -> Maybe AccountId -> List Share -> State -> State
waitFor key fillForm shares =
    updateGroupPws
        (Dict.insert key
            (tryGetPassword key
                shares
                { fillForm = fillForm
                , shares = Set.empty
                , password = Nothing
                }
            )
        )


getWaiting : State -> List GroupId
getWaiting (State state) =
    Dict.filter
        (\_ info -> isWaiting info)
        state.groupPws
        |> Dict.keys


isWaiting : Info -> Bool
isWaiting info =
    info.password == Nothing


removeWaiting : State -> State
removeWaiting =
    updateGroupPws
        (Dict.filter (\_ info -> not (isWaiting info)))


removeWaitingGroups : List GroupId -> State -> State
removeWaitingGroups groups =
    updateGroupPws
        (Dict.filter (\groupId info -> not (isWaiting info && List.member groupId groups)))


updateGroupPws fn (State ({ groupPws } as state)) =
    State { state | groupPws = fn groupPws }


updatePws fn (State ({ pws } as state)) =
    State { state | pws = fn pws }


unlockIfPossible : GroupId -> List Share -> State -> State
unlockIfPossible gId shares state =
    updateGroupPws
        (Dict.update gId
            (Maybe.map (\info -> tryGetPassword gId shares info))
        )
        state


addShare : GroupId -> List Share -> Share -> State -> ( State, Maybe AccountId )
addShare key shares share state =
    let
        newState =
            updateGroupPws
                (Dict.update key
                    (Maybe.map (\info -> tryGetPassword key shares { info | shares = Set.insert share info.shares }))
                )
                state
    in
        case getStatus key shares newState of
            Done (Just accountId) groupPw ->
                ( newState, Just accountId )

            _ ->
                ( newState, Nothing )


type PasswordStatus
    = WaitForUnlockGroup Int Int
    | Locked
    | UnlockedButHidden
    | Unlocked Password


isUnlocked : PasswordStatus -> Bool
isUnlocked status =
    case status of
        Unlocked _ ->
            True

        UnlockedButHidden ->
            True

        _ ->
            False


getPassword : (GroupId -> List Share) -> AccountId -> Maybe ( GroupId, EncryptedPassword ) -> Maybe Password -> State -> Maybe Password
getPassword getShares accountId mayEncPw mayPw state =
    tryGetAccountPassword getShares accountId mayEncPw mayPw False state
        |> (\(State s) -> Dict.get accountId s.pws)
        |> Maybe.map Tuple.first


getPwStatus : AccountId -> GroupId -> List Share -> State -> PasswordStatus
getPwStatus accountId groupId shares (State state) =
    case Dict.get accountId state.pws of
        Just ( pw, doShow ) ->
            if doShow then
                Unlocked pw
            else
                UnlockedButHidden

        Nothing ->
            case mayInfoToStatus groupId shares (Dict.get groupId state.groupPws) of
                Done _ _ ->
                    UnlockedButHidden

                Waiting n m ->
                    WaitForUnlockGroup n m

                _ ->
                    Locked


togglePassword : (GroupId -> List Share) -> AccountId -> Maybe ( GroupId, EncryptedPassword ) -> Maybe Password -> State -> State
togglePassword getShares accountId mayEncPw mayPw state =
    tryGetAccountPassword getShares accountId mayEncPw mayPw False state
        |> updatePws (Dict.update accountId (Maybe.map (\( pw, shouldShow ) -> ( pw, not shouldShow ))))


hasPwInCache : AccountId -> State -> Bool
hasPwInCache accountId (State state) =
    Dict.member accountId state.pws


tryGetAccountPassword : (GroupId -> List Share) -> AccountId -> Maybe ( GroupId, EncryptedPassword ) -> Maybe Password -> Bool -> State -> State
tryGetAccountPassword getShares accountId mayEncPw mayPw shouldShow state =
    if hasPwInCache accountId state then
        state
    else
        case mayPw of
            Just pw ->
                cacheAccountPw accountId pw shouldShow state

            Nothing ->
                case mayEncPw of
                    Just ( groupId, encPw ) ->
                        case getStatus groupId (getShares groupId) state of
                            Done _ groupPw ->
                                case AES.decryptPassword groupPw encPw of
                                    Ok pw ->
                                        cacheAccountPw accountId pw shouldShow state

                                    Err e ->
                                        Debug.log "Why can we not decrypt???" e
                                            |> always state

                            _ ->
                                state

                    Nothing ->
                        state


cacheAccountPw : AccountId -> Password -> Bool -> State -> State
cacheAccountPw accountId pw shouldShow (State ({ pws } as state)) =
    State { state | pws = Dict.insert accountId ( pw, shouldShow ) pws }


invalidatePwCache : AccountId -> State -> State
invalidatePwCache accountId (State ({ pws } as state)) =
    State { state | pws = Dict.remove accountId pws }


invalidatePwCaches : State -> State
invalidatePwCaches (State ({ pws } as state)) =
    State { state | pws = Dict.empty }


invalidatePwCacheIfExists : AccountId -> State -> State
invalidatePwCacheIfExists accountId (State ({ pws } as state)) =
    if Dict.member accountId pws then
        invalidatePwCache accountId (State state)
    else
        State state


cacheGroupPw : GroupId -> GroupPassword -> State -> State
cacheGroupPw groupId groupPw (State state) =
    State
        { state
            | groupPws =
                Dict.update groupId
                    (\mayInfo ->
                        case mayInfo of
                            Just info ->
                                Just { info | password = Just (Ok groupPw) }

                            Nothing ->
                                Just { fillForm = Nothing, shares = Set.empty, password = Just (Ok groupPw) }
                    )
                    state.groupPws
        }


tryGetPassword : GroupId -> List Share -> Info -> Info
tryGetPassword ( level, _ ) shares info =
    case info.password of
        Just pw ->
            info

        Nothing ->
            if Set.size info.shares + List.length shares >= level then
                -- expensive operation
                { info | password = Just <| SecretSharing.joinToBytes (Set.toList info.shares ++ shares) }
            else
                info
