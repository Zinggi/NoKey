module SyncData exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random.Pcg as Random exposing (Seed)


--

import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.SingleVersionRegister as SingleVersionRegister exposing (SingleVersionRegister)


type alias SyncData =
    { id : String, knownIds : ORDict String (SingleVersionRegister String), synchedWith : Set String }


init : Seed -> String -> SyncData
init seed uuid =
    { knownIds = ORDict.init seed |> ORDict.insert uuid (SingleVersionRegister.init ""), synchedWith = Set.empty, id = uuid }


removeDevice : String -> SyncData -> SyncData
removeDevice uuid sync =
    { sync | knownIds = ORDict.remove uuid sync.knownIds, synchedWith = Set.empty }


gotRemoved : SyncData -> SyncData
gotRemoved sync =
    let
        myName =
            ORDict.get sync.knownIds |> Dict.get sync.id |> Maybe.map Tuple.second |> Maybe.withDefault ""
    in
        { sync | knownIds = ORDict.reset sync.knownIds |> ORDict.insert sync.id (SingleVersionRegister.init myName), synchedWith = Set.empty }


renameDevice : String -> SyncData -> SyncData
renameDevice newName sync =
    { sync | knownIds = ORDict.update sync.id (SingleVersionRegister.update newName) sync.knownIds, synchedWith = Set.empty }


pairedWith : String -> SyncData -> SyncData -> SyncData
pairedWith uuid hisSync mySync =
    case Dict.get uuid (ORDict.get hisSync.knownIds) of
        Just v ->
            { mySync | knownIds = ORDict.insert uuid v mySync.knownIds }

        Nothing ->
            mySync


{-| **CAUTION**
The order of arguments matter, e.g.
`newA = merge b a` means merge b into a to produce newA
-}
merge : SyncData -> SyncData -> SyncData
merge other my =
    let
        newData =
            ORDict.merge
                SingleVersionRegister.merge
                other.knownIds
                my.knownIds
    in
        { my
            | knownIds = newData
            , synchedWith =
                (if not <| ORDict.equal newData my.knownIds then
                    Set.empty
                 else
                    (if ORDict.equal other.knownIds newData then
                        my.synchedWith
                     else
                        Set.remove other.id my.synchedWith
                    )
                )
                    |> (\sWith ->
                            if ORDict.equal newData other.knownIds then
                                Set.insert other.id sWith
                            else
                                sWith
                       )
        }


getContactSet : SyncData -> Set String
getContactSet sync =
    Set.diff (ORDict.get sync.knownIds |> Dict.keys |> Set.fromList) (Set.insert sync.id sync.synchedWith)


updateSynchedWith : Set String -> SyncData -> SyncData
updateSynchedWith s sync =
    { sync | synchedWith = Set.union s sync.synchedWith }


resetSynchedWith : SyncData -> SyncData
resetSynchedWith sync =
    { sync | synchedWith = Set.empty }


decoder : Decoder SyncData
decoder =
    JD.map3 SyncData
        -- TODO: don't decode id, get it from the sender
        (JD.field "id" JD.string)
        (JD.field "knownIds" <| ORDict.decoder (SingleVersionRegister.decoder JD.string))
        (JD.succeed Set.empty)


encode : SyncData -> Value
encode s =
    -- TODO: don't encode ID
    JE.object [ ( "knownIds", ORDict.encode (SingleVersionRegister.encode JE.string) s.knownIds ), ( "id", JE.string s.id ) ]
