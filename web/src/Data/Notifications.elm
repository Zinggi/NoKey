module Data.Notifications exposing (Notification(..), Notifications, Id, ShareRequest, SiteEntry, newSiteEntry, first, init, remove, newShareRequest, count, map)

import Dict exposing (Dict)


type Notification
    = ShareRequestT ShareRequest
      -- TODO: This notification has to persist,
      -- e.g. we never want to lose a password just because we closed the browser
    | ExternalSiteEntry SiteEntry Bool


type alias SiteEntry =
    { password : String
    , login : String
    , site : String
    , securityLevel : Int
    }


type alias Id =
    Int


type alias Notifications =
    { data : Dict Int Notification
    , maxKey : Int
    }


type alias ShareRequest =
    { id : String
    , key : ( String, String )
    }


init : Notifications
init =
    { data = Dict.empty, maxKey = 0 }


newSiteEntry : SiteEntry -> Bool -> Notifications -> Notifications
newSiteEntry entry isNew ns =
    { ns | data = Dict.insert ns.maxKey (ExternalSiteEntry entry isNew) ns.data, maxKey = ns.maxKey + 1 }


count : Notifications -> Int
count ns =
    Dict.size ns.data


first : Notifications -> Maybe ( Id, Notification )
first ns =
    Dict.toList ns.data |> List.head


remove : Id -> Notifications -> Notifications
remove id ns =
    { ns | data = Dict.remove id ns.data }


newShareRequest : String -> ( String, String ) -> Notifications -> Notifications
newShareRequest id key ns =
    { ns | data = Dict.insert ns.maxKey (ShareRequestT { id = id, key = key }) ns.data, maxKey = ns.maxKey + 1 }


map : (Id -> Notification -> b) -> Notifications -> List b
map f ns =
    Dict.toList ns.data
        |> List.map (\( id, n ) -> f id n)
