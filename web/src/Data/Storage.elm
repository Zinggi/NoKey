module Data.Storage exposing (State, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Data.Sync as Sync exposing (SyncData)
import Json.Decode.Pipeline as JD exposing (required, optional, decode)
import Data.Options exposing (Options)


type alias State a =
    { a | syncData : SyncData, uniqueIdentifyier : String, isFirstTimeUser : Bool, options : Options }


stateDecoder : Decoder (State {})
stateDecoder =
    JD.decode
        (\sync id ftu options ->
            { syncData = sync
            , uniqueIdentifyier = id
            , isFirstTimeUser = ftu
            , options = options
            }
        )
        |> required "syncData" Sync.completeDecoder
        |> required "uuid" JD.string
        |> optional "isFirstTimeUser" JD.bool False
        |> optional "options" Data.Options.decoder Data.Options.defaults


encode : State a -> Value
encode { syncData, uniqueIdentifyier, isFirstTimeUser, options } =
    JE.object
        [ ( "syncData", Sync.encodeComplete syncData )
        , ( "uuid", JE.string uniqueIdentifyier )
        , ( "isFirstTimeUser", JE.bool isFirstTimeUser )
        , ( "options", Data.Options.encode options )
        ]


decode : Value -> Result String (State {})
decode state =
    JD.decodeValue stateDecoder state
