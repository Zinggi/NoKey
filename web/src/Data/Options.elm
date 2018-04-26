module Data.Options exposing (Options, encode, decoder, defaults, setAllowLevel1, minSecurityLevel)

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD exposing (required, optional)
import Time exposing (Time)


type alias Options =
    { allowLevel1 : Bool
    , timeUntilAutoLock : Time
    }


setAllowLevel1 : Bool -> Options -> Options
setAllowLevel1 b o =
    { o | allowLevel1 = b }


minSecurityLevel : Options -> Int
minSecurityLevel options =
    if options.allowLevel1 then
        1
    else
        2


defaults : Options
defaults =
    { allowLevel1 = False, timeUntilAutoLock = 10 * Time.minute }


encode : Options -> Value
encode opt =
    JE.object
        [ ( "allowLevel1", JE.bool opt.allowLevel1 )
        , ( "timeUntilAutoLock", JE.float opt.timeUntilAutoLock )
        ]


decoder : Decoder Options
decoder =
    JD.decode (\al1 t -> { allowLevel1 = al1, timeUntilAutoLock = t })
        |> optional "allowLevel1" JD.bool defaults.allowLevel1
        |> optional "timeUntilAutoLock" JD.float defaults.timeUntilAutoLock
