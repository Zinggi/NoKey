port module Ports exposing (setTitle, storeState, resetStorage)

import Json.Encode exposing (Value)


port setTitle : String -> Cmd msg


port storeState : Value -> Cmd msg


port resetStorage : () -> Cmd msg



-- port onStateChange : (Value -> msg) -> Sub msg
