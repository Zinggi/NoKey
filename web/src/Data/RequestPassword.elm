module Data.RequestPassword exposing (State, Status(..), init, getStatus, addShare, waitFor, getStatusForSite)

import Dict exposing (Dict)
import SecretSharing exposing (Share)
import Helper exposing (maybeToList)


type State
    = State (Dict ( String, String ) Info)


type alias Info =
    { requiredParts : Int, fillForm : Bool, shares : List Share, password : Maybe (Result String String) }


type Status
    = NotRequested
    | Waiting Int Int
    | Done Bool String
    | Error String


init : State
init =
    State Dict.empty


getStatus : ( String, String ) -> State -> Status
getStatus key (State state) =
    mayInfoToStatus (Dict.get key state)


mayInfoToStatus : Maybe Info -> Status
mayInfoToStatus mayInfo =
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
                    Waiting (List.length info.shares) info.requiredParts


waitFor : ( String, String ) -> Bool -> Int -> Maybe Share -> State -> State
waitFor key fillForm requiredParts maybeMyShare (State state) =
    State <|
        Dict.insert key
            (tryGetPassword
                { fillForm = fillForm
                , shares = maybeToList maybeMyShare
                , requiredParts = requiredParts
                , password = Nothing
                }
            )
            state


getStatusForSite : String -> State -> Dict String Status
getStatusForSite site (State state) =
    Dict.filter (\( siteName, _ ) info -> siteName == site) state
        |> Dict.foldl
            (\( siteName, loginName ) info acc ->
                Dict.insert loginName (mayInfoToStatus (Just info)) acc
            )
            Dict.empty


addShare : ( String, String ) -> Share -> State -> State
addShare key share (State state) =
    (State <|
        Dict.update key
            (Maybe.map (\info -> tryGetPassword { info | shares = share :: info.shares }))
            state
    )


tryGetPassword : Info -> Info
tryGetPassword info =
    case info.password of
        Just pw ->
            info

        Nothing ->
            if (List.length info.shares) >= info.requiredParts then
                -- expensive operation
                { info | password = Just <| SecretSharing.joinToString info.shares }
            else
                info
