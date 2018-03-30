port module Ports exposing (..)

import Time exposing (Time)
import Json.Encode exposing (Value)
import Data.Notifications exposing (SiteEntry)
import Data exposing (GroupId, DeviceId)


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



-- Crypto stuff
----------------------------------------------------------------------------------
-- Sign / verify


port verifyAuthenticity : { time : Time, from : String, data : Value, signature : Value, key : Value } -> Cmd msg


port onAuthenticatedMsg : ({ data : Value, isAuthentic : Bool, time : Time, from : String } -> msg) -> Sub msg


port getSignatureForMsg : { msg : Value, otherId : String } -> Cmd msg


port onSignedMsg : ({ data : Value, signature : Value, otherId : String } -> msg) -> Sub msg



-- encrypt / decrypt


port decryptMyShares : List ( GroupId, Value ) -> Cmd msg


port onReceiveMyShares : (List ( GroupId, Value ) -> msg) -> Sub msg


port encryptNewShares : { time : Time, groupId : GroupId, shares : List ( DeviceId, ( Value, Value ) ) } -> Cmd msg


port onNewEncryptedShares : ({ time : Time, groupId : GroupId, shares : List ( DeviceId, Value ) } -> msg) -> Sub msg


{-| used for exchanging our shares
-}
port encryptShares : { shares : List ( GroupId, Value ), publicKey : Value, deviceId : DeviceId, reqIds : Value } -> Cmd msg


type alias DidEncryptSharesT =
    { deviceId : DeviceId, encryptedShares : Value, reqIds : Value }


port onDidEncryptShares : (DidEncryptSharesT -> msg) -> Sub msg


type alias DidDecryptRequestedSharesT =
    { shares : Value, time : Time, otherId : DeviceId, ids : List String }


port decryptRequestedShares : { ids : List String, shares : Value, time : Time, otherId : DeviceId } -> Cmd msg


port onDidDecryptRequestedShares : (DidDecryptRequestedSharesT -> msg) -> Sub msg
