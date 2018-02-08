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


filter : (Notification -> Bool) -> Notifications -> List Notification
filter f ns =
    Dict.foldl
        (\id n acc ->
            if f n then
                n :: acc
            else
                acc
        )
        []
        ns.data


newSiteEntry : SiteEntry -> Bool -> Notifications -> Notifications
newSiteEntry entry isNew ns =
    -- Only keep the most recent entry for a specific site.
    replaceIfElseInsert
        (\n ->
            case n of
                ExternalSiteEntry ent isN ->
                    ent.site == entry.site

                _ ->
                    False
        )
        (ExternalSiteEntry entry isNew)
        ns


insert : Notification -> Notifications -> Notifications
insert n ns =
    { ns | data = Dict.insert ns.maxKey n ns.data, maxKey = ns.maxKey + 1 }


member : Notification -> Notifications -> Bool
member n ns =
    Dict.filter (\id n_ -> n_ == n) ns.data
        |> Dict.size
        |> (\x -> x > 0)


replaceIfElseInsert : (Notification -> Bool) -> Notification -> Notifications -> Notifications
replaceIfElseInsert f n ns =
    let
        reducer id n_ ( acc, didReplace ) =
            if f n_ then
                ( Dict.insert id n acc, True )
            else
                ( Dict.insert id n_ acc, didReplace )

        ( newData, didReplace ) =
            Dict.foldl reducer ( Dict.empty, False ) ns.data
    in
        if didReplace then
            { ns | data = newData }
        else
            insert n ns


insertNoDuplicate : Notification -> Notifications -> Notifications
insertNoDuplicate n ns =
    if member n ns then
        ns
    else
        insert n ns


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
    insertNoDuplicate (ShareRequestT { id = id, key = key }) ns


map : (Id -> Notification -> b) -> Notifications -> List b
map f ns =
    Dict.toList ns.data
        |> List.map (\( id, n ) -> f id n)
