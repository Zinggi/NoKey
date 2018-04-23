module Data.Notifications
    exposing
        ( Notification
        , Notifications
        , NotificationT(..)
        , Id
        , ShareRequest
        , SiteEntry
        , newSiteEntry
        , first
        , init
        , remove
        , newShareRequest
        , newShareRequestWithId
        , count
        , map
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Data exposing (GroupId, DeviceId)


type alias Notification =
    { id : Id, data : NotificationT }


type NotificationT
    = ShareRequestT ShareRequest
      -- TODO: This notification has to persist,
      -- e.g. we never want to lose a password just because we closed the browser
      -- But should it really? next time we might not even remember we wanted to add it
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
    { keys : Set GroupId
    , deviceId : DeviceId
    , reqIds : Set String
    }


init : Notifications
init =
    { data = Dict.empty, maxKey = 0 }


newSiteEntry : SiteEntry -> Bool -> Notifications -> Notifications
newSiteEntry entry isNew ns =
    if List.any ((==) "") [ entry.password, entry.login, entry.site ] then
        ns
    else
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
            |> Tuple.second


insert : NotificationT -> Notifications -> Notifications
insert n ns =
    { ns | data = Dict.insert ns.maxKey { data = n, id = ns.maxKey } ns.data, maxKey = ns.maxKey + 1 }


insertWithId : NotificationT -> Notifications -> ( Id, Notifications )
insertWithId n ns =
    ( ns.maxKey, insert n ns )


updateIfOrInsert : (NotificationT -> Bool) -> (NotificationT -> NotificationT) -> NotificationT -> Notifications -> ( Int, Notifications )
updateIfOrInsert shouldUpdate update n ns =
    let
        reducer id n_ ( acc, didReplace ) =
            if shouldUpdate n_.data then
                ( Dict.insert id { n_ | data = update n_.data } acc, Just id )
            else
                ( Dict.insert id n_ acc, didReplace )

        ( newData, didReplace ) =
            Dict.foldl reducer ( Dict.empty, Nothing ) ns.data
    in
        case didReplace of
            Just id ->
                ( id, { ns | data = newData } )

            Nothing ->
                insertWithId n ns


replaceIfElseInsert : (NotificationT -> Bool) -> NotificationT -> Notifications -> ( Int, Notifications )
replaceIfElseInsert f n ns =
    updateIfOrInsert f (always n) n ns


count : Notifications -> Int
count ns =
    Dict.size ns.data


first : Notifications -> Maybe Notification
first ns =
    Dict.values ns.data |> List.head


remove : Id -> Notifications -> Notifications
remove id ns =
    { ns | data = Dict.remove id ns.data }


newShareRequest : String -> DeviceId -> List GroupId -> Notifications -> Notifications
newShareRequest reqId id keys ns =
    Tuple.second (newShareRequestWithId reqId id keys ns)


newShareRequestWithId : String -> DeviceId -> List GroupId -> Notifications -> ( Id, Notifications )
newShareRequestWithId reqId id keys ns =
    let
        newKeys =
            Set.fromList keys

        newEntry =
            ShareRequestT { deviceId = id, reqIds = Set.singleton reqId, keys = newKeys }
    in
        updateIfOrInsert
            (\n ->
                case n of
                    ShareRequestT r ->
                        r.deviceId == id

                    _ ->
                        False
            )
            (\n ->
                case n of
                    ShareRequestT r ->
                        ShareRequestT { r | reqIds = Set.insert reqId r.reqIds, keys = Set.union newKeys r.keys }

                    _ ->
                        newEntry
            )
            newEntry
            ns


map : (Id -> Notification -> b) -> Notifications -> List b
map f ns =
    Dict.toList ns.data
        |> List.map (\( id, n ) -> f id n)
