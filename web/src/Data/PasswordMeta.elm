module Data.PasswordMeta exposing (PasswordMetaData, default, reset)


type alias PasswordMetaData =
    { securityLevel : Int
    , length : Int
    , siteName : String
    , userName : String
    }


default : PasswordMetaData
default =
    { securityLevel = 2, length = 16, siteName = "", userName = "" }


reset : PasswordMetaData -> PasswordMetaData
reset meta =
    { meta | siteName = "", securityLevel = 2, length = 16 }
