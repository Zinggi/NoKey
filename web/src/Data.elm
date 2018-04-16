module Data exposing (..)

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD exposing (required)
import Time exposing (Time)
import Helper exposing (encodeTuple, encodeTuple2, decodeTuple, decodeTuple2)


-- Settings


type alias Settings =
    { timeUntilAutoLock : Time
    }


defaultSettings : Settings
defaultSettings =
    { timeUntilAutoLock = 10 * Time.minute }


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.decode (\t -> { timeUntilAutoLock = t })
        |> required "timeUntilAutoLock" JD.float


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object [ ( "timeUntilAutoLock", JE.float settings.timeUntilAutoLock ) ]



-- For ports


type alias FillFormData =
    { login : String, site : String, password : String }



-- Common Data


type alias DeviceId =
    String


type alias Device =
    { id : DeviceId, name : String, postFix : String }


type alias Password =
    String


{-| (SecurityLevel, CreatorId)
-}
type alias GroupId =
    ( Int, String )


encodeGroupId : GroupId -> Value
encodeGroupId id =
    encodeTuple2 JE.int JE.string id


groupIdDecoder : Decoder GroupId
groupIdDecoder =
    decodeTuple2 JD.int JD.string


type alias AccountId =
    -- siteName, userName
    ( String, String )


encodeAccountId : AccountId -> Value
encodeAccountId id =
    encodeTuple JE.string id


accountIdDecoder : Decoder AccountId
accountIdDecoder =
    decodeTuple JD.string


type EncryptedPassword
    = EncryptedPassword String


encodeEncryptedPassword : EncryptedPassword -> Value
encodeEncryptedPassword (EncryptedPassword pw) =
    JE.string pw


encryptedPasswordDecoder : Decoder EncryptedPassword
encryptedPasswordDecoder =
    JD.map EncryptedPassword JD.string


type alias GroupPassword =
    List Int
