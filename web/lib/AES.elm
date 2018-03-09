module AES exposing (..)

import Crypto.Strings as AES
import Random as RandomC
import Time exposing (Time)
import Data exposing (..)


encryptPassword : Time -> GroupPassword -> Password -> Result String EncryptedPassword
encryptPassword time groupPw pw =
    AES.encrypt (RandomC.initialSeed (round time)) groupPw pw
        |> Result.map (\( encPw, seedC ) -> EncryptedPassword encPw)


decryptPassword : GroupPassword -> EncryptedPassword -> Result String Password
decryptPassword groupPw (EncryptedPassword encryptedPw) =
    AES.decrypt groupPw encryptedPw
