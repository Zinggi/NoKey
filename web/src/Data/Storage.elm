module Data.Storage exposing (State, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Data.Sync as Sync exposing (SyncData)


type alias State a =
    { a | syncData : SyncData, uniqueIdentifyier : String }


stateDecoder : Decoder (State {})
stateDecoder =
    JD.map2 (\sync id -> { syncData = sync, uniqueIdentifyier = id })
        (JD.field "syncData" Sync.completeDecoder)
        (JD.field "uuid" JD.string)


encode : State a -> Value
encode { syncData, uniqueIdentifyier } =
    JE.object
        [ ( "syncData", Sync.encodeComplete syncData )
        , ( "uuid", JE.string uniqueIdentifyier )
        ]


decode : Value -> Result String (State {})
decode state =
    JD.decodeValue stateDecoder state
