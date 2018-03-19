port module Ports exposing (..)

import Json.Encode exposing (Value)
import Data.Notifications exposing (SiteEntry)
import Data exposing (GroupId)


port setTitle : String -> Cmd msg


port storeState : Value -> Cmd msg


{-| the js side is expected to reset the storage and store the newly provided state instead
-}
port resetStorage : Value -> Cmd msg


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
port accountsForSite : List ( String, GroupId ) -> Cmd msg


{-| gets called when we register to a new site
-}
port onAddSiteEntry : ({ isSignUp : Bool, entry : SiteEntry } -> msg) -> Sub msg


{-| Call this whenever the notification count changes
-}
port notificationCount : Int -> Cmd msg


{-| should get called after we requested to get a password and fill the form
-}
port fillForm : { login : String, site : String, password : String } -> Cmd msg


{-| indicate to the popup that it should close itself
-}
port closePopup : () -> Cmd msg
