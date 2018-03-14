module Data exposing (..)

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Helper exposing (encodeTuple, encodeTuple2, decodeTuple, decodeTuple2)


-- For ports


type alias FillFormData =
    { login : String, site : String, password : String }



-- Common Data


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
    String
