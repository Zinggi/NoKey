port module Ports exposing (setTitle, storeState)

import Json.Encode exposing (Value)


port setTitle : String -> Cmd msg


port storeState : Value -> Cmd msg



-- port onStateChange : (Value -> msg) -> Sub msg
