module Data.Notifications exposing (Notification(..), Notifications, Id, ShareRequest, init, remove, newShareRequest, map)

import Dict exposing (Dict)


type Notification
    = ShareRequestT ShareRequest


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
