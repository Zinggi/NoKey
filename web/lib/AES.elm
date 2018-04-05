module AES exposing (..)

import Crypto.Strings as AES
import Crypto.Strings.Encoding as Enc
import Random as RandomC
import Time exposing (Time)
import Data exposing (..)


encryptPassword : Time -> GroupPassword -> Password -> Result String EncryptedPassword
encryptPassword time groupPw pw =
    AES.encrypt (RandomC.initialSeed (round time)) (Enc.base64Encoder 0 groupPw) pw
        |> Result.map (\( encPw, seedC ) -> EncryptedPassword encPw)


decryptPassword : GroupPassword -> EncryptedPassword -> Result String Password
decryptPassword groupPw (EncryptedPassword encryptedPw) =
    AES.decrypt (Enc.base64Encoder 0 groupPw) encryptedPw
