module Data.RequestPassword exposing (State)

import Dict exposing (Dict)
import SecretSharing exposing (Share)


type alias State =
    Dict ( String, String ) (List SecretSharing.Share)
