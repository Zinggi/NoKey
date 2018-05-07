module Data.PasswordMeta exposing (PasswordMetaData, default, reset, setSiteName)


type alias PasswordMetaData =
    { securityLevel : Int
    , length : Int
    , siteName : String
    , userName : String
    , counter : Int
    }


default : PasswordMetaData
default =
    { securityLevel = 2, length = 16, siteName = "", userName = "", counter = 0 }


{-| call this when setting the site name from something different than user input
-}
setSiteName : String -> PasswordMetaData -> PasswordMetaData
setSiteName s m =
    { m | siteName = s, counter = m.counter + 1 }


reset : PasswordMetaData -> PasswordMetaData
reset meta =
    { meta | securityLevel = 2, length = 16 } |> setSiteName ""
