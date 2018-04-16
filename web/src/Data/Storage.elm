module Data.Storage exposing (State, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Data.Sync as Sync exposing (SyncData)
import Json.Decode.Pipeline as JD exposing (required, optional, decode)
import Data exposing (Settings)


type alias State a =
    { a | syncData : SyncData, uniqueIdentifyier : String, isFirstTimeUser : Bool, settings : Settings }


stateDecoder : Decoder (State {})
stateDecoder =
    JD.decode
        (\sync id ftu settings ->
            { syncData = sync
            , uniqueIdentifyier = id
            , isFirstTimeUser = ftu
            , settings = settings
            }
        )
        |> required "syncData" Sync.completeDecoder
        |> required "uuid" JD.string
        |> optional "isFirstTimeUser" JD.bool False
        |> optional "settings" Data.settingsDecoder Data.defaultSettings


encode : State a -> Value
encode { syncData, uniqueIdentifyier, isFirstTimeUser, settings } =
    JE.object
        [ ( "syncData", Sync.encodeComplete syncData )
        , ( "uuid", JE.string uniqueIdentifyier )
        , ( "isFirstTimeUser", JE.bool isFirstTimeUser )
        , ( "settings", Data.encodeSettings settings )
        ]


decode : Value -> Result String (State {})
decode state =
    JD.decodeValue stateDecoder state
