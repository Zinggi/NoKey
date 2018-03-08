module Data exposing (..)

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


type alias AccountId =
    -- siteName, userName
    ( String, String )


type EncryptedPassword
    = EncryptedPassword String


type alias GroupPassword =
    String
