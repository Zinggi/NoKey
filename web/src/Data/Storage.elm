module Data.Storage exposing (State, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random.Pcg as Random exposing (Seed)
import Data.Sync as Sync exposing (SyncData)
import Json.Decode.Pipeline as JD exposing (required, optional, decode)


type alias State a =
    { a | syncData : SyncData, uniqueIdentifyier : String, isFirstTimeUser : Bool }


stateDecoder : Seed -> Decoder (State {})
stateDecoder seed =
    JD.decode
        (\sync id ftu ->
            { syncData = sync
            , uniqueIdentifyier = id
            , isFirstTimeUser = ftu
            }
        )
        |> required "syncData" (Sync.completeDecoder seed)
        |> required "uuid" JD.string
        |> optional "isFirstTimeUser" JD.bool False


encode : State a -> Value
encode { syncData, uniqueIdentifyier, isFirstTimeUser } =
    JE.object
        [ ( "syncData", Sync.encodeComplete syncData )
        , ( "uuid", JE.string uniqueIdentifyier )
        , ( "isFirstTimeUser", JE.bool False )
        ]


decode : Seed -> Value -> Result String (State {})
decode seed state =
    JD.decodeValue (stateDecoder seed) state
