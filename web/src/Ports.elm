port module Ports exposing (..)

import Json.Encode exposing (Value)
import Data.Notifications exposing (SiteEntry)


port setTitle : String -> Cmd msg


port storeState : Value -> Cmd msg


port resetStorage : () -> Cmd msg


{-| we should call this whenever there is a new state, this is used to inform other views of our state
-}
port sendOutNewState : Value -> Cmd msg


{-| this should be answered with `sendOutNewState`
-}
port onStateRequest : ({} -> msg) -> Sub msg


{-| this gets called when another view wants to send a msg to the background app
-}
port onReceiveMsg : (Value -> msg) -> Sub msg


{-| This should be answered with accountsForSite
-}
port onRequestAccountsForSite : (String -> msg) -> Sub msg


{-| This sends out the accounts that we have saved for a site, as an answer to onRequestAccountsForSite
-}
port accountsForSite : List String -> Cmd msg


{-| gets called when we register to a new site
-}
port onAddSiteEntry : (SiteEntry -> msg) -> Sub msg


{-| Call this whenever the notification count changes
-}
port notificationCount : Int -> Cmd msg



-- port onStateChange : (Value -> msg) -> Sub msg
