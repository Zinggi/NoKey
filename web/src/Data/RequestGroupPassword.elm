module Data.RequestGroupPassword
    exposing
        ( State
        , Status(..)
        , PasswordStatus(..)
        , init
        , getStatus
        , getPwStatus
        , togglePassword
        , addShare
        , waitFor
        , getWaiting
        , removeWaiting
        , cacheAccountPw
        , cacheGroupPw
        , getGroupPassword
        )

import Dict exposing (Dict)
import AES
import SecretSharing exposing (Share)
import Data exposing (GroupId, AccountId, GroupPassword, FillFormData, Password, EncryptedPassword)
import Helper exposing (maybeToList)


type State
    = State { groupPws : Dict GroupId Info, pws : Dict AccountId ( Password, Bool ) }


type alias Info =
    { fillForm : Maybe AccountId, shares : List Share, password : Maybe (Result String String) }


type Status
    = NotRequested
    | Waiting Int Int
    | Done (Maybe AccountId) GroupPassword
    | Error String


init : State
init =
    State { groupPws = Dict.empty, pws = Dict.empty }


getGroupPassword : GroupId -> State -> Maybe GroupPassword
getGroupPassword groupId state =
    case getStatus groupId state of
        Done _ pw ->
            Just pw

        _ ->
            Nothing


getStatus : GroupId -> State -> Status
getStatus key (State state) =
    mayInfoToStatus key (Dict.get key state.groupPws)


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
waitFor key fillForm maybeMyShare =
    updateGroupPws
        (Dict.insert key
            (tryGetPassword key
                { fillForm = fillForm
                , shares = maybeToList maybeMyShare
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


updateGroupPws fn (State ({ groupPws } as state)) =
    State { state | groupPws = fn groupPws }


updatePws fn (State ({ pws } as state)) =
    State { state | pws = fn pws }


addShare : GroupId -> Share -> State -> ( State, Maybe FillFormData )
addShare key share state =
    let
        newState =
            updateGroupPws
                (Dict.update key
                    (Maybe.map (\info -> tryGetPassword key { info | shares = share :: info.shares }))
                )
                state
    in
        case getStatus key newState of
            Done (Just ( site, login )) pw ->
                ( newState, Just { login = login, site = site, password = pw } )

            _ ->
                ( newState, Nothing )


type PasswordStatus
    = WaitForUnlockGroup Int Int
    | Locked
    | UnlockedButHidden
    | Unlocked Password


getPwStatus : AccountId -> GroupId -> State -> PasswordStatus
getPwStatus accountId groupId (State state) =
    case Dict.get accountId state.pws of
        Just ( pw, doShow ) ->
            if doShow then
                Unlocked pw
            else
                UnlockedButHidden

        Nothing ->
            case mayInfoToStatus groupId (Dict.get groupId state.groupPws) of
                Done _ _ ->
                    UnlockedButHidden

                Waiting n m ->
                    WaitForUnlockGroup n m

                _ ->
                    Locked


togglePassword : AccountId -> Maybe ( GroupId, EncryptedPassword ) -> Maybe Password -> State -> State
togglePassword accountId mayEncPw mayPw state =
    tryGetAccountPassword accountId mayEncPw mayPw False state
        |> updatePws (Dict.update accountId (Maybe.map (\( pw, shouldShow ) -> ( pw, not shouldShow ))))


hasPwInCache : AccountId -> State -> Bool
hasPwInCache accountId (State state) =
    Dict.member accountId state.pws


tryGetAccountPassword : AccountId -> Maybe ( GroupId, EncryptedPassword ) -> Maybe Password -> Bool -> State -> State
tryGetAccountPassword accountId mayEncPw mayPw shouldShow state =
    if hasPwInCache accountId state then
        state
    else
        case mayPw of
            Just pw ->
                cacheAccountPw accountId pw shouldShow state

            Nothing ->
                case mayEncPw of
                    Just ( groupId, encPw ) ->
                        case getStatus groupId state of
                            Done _ groupPw ->
                                case AES.decryptPassword groupPw encPw of
                                    Ok pw ->
                                        cacheAccountPw accountId pw shouldShow state

                                    Err e ->
                                        -- TODO: when does this happen? We probably shouldn't crash
                                        Debug.crash ("Why can we not decrypt???" ++ e)

                            _ ->
                                state

                    Nothing ->
                        state


cacheAccountPw : AccountId -> Password -> Bool -> State -> State
cacheAccountPw accountId pw shouldShow (State ({ pws } as state)) =
    State { state | pws = Dict.insert accountId ( pw, shouldShow ) pws }


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
                                Just { fillForm = Nothing, shares = [], password = Just (Ok groupPw) }
                    )
                    state.groupPws
        }


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
