module PortUtils exposing (toJs, fromJs)

import Json.Encode exposing (Value)
import Native.PortUtils


toJs : a -> Value
toJs =
    Native.PortUtils.toJs


fromJs : Value -> a
fromJs =
    Native.PortUtils.fromJs
