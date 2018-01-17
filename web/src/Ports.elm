port module Ports exposing (setTitle, storeState, resetStorage, onStateRequest, sendOutNewState, onReceiveMsg)

import Json.Encode exposing (Value)


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



-- port onStateChange : (Value -> msg) -> Sub msg
