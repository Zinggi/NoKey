module AES exposing (..)

import Crypto.Strings as AES
import Crypto.Strings.Encoding as Enc
import Random as RandomC
import Random.Pcg as Random exposing (Seed)
import Time exposing (Time)
import Data exposing (..)
import Helper exposing (pcgToCore, coreToPcg)
import SecretSharing
import BigInt exposing (BigInt)


encryptPassword : Seed -> GroupPassword -> Password -> Result String ( EncryptedPassword, Seed )
encryptPassword seed groupPw pw =
    AES.encrypt (pcgToCore seed) (Enc.base64Encoder 0 groupPw) pw
        |> Result.map (\( encPw, seedC ) -> ( EncryptedPassword encPw, coreToPcg seedC ))


decryptPassword : GroupPassword -> EncryptedPassword -> Result String Password
decryptPassword groupPw (EncryptedPassword encryptedPw) =
    AES.decrypt (Enc.base64Encoder 0 groupPw) encryptedPw


encryptShare : Seed -> String -> SecretSharing.Share -> Result String ( SecretSharing.EncryptedShare, Seed )
encryptShare seed key share =
    case AES.encrypt (pcgToCore seed) key (BigInt.toString share.y) of
        Ok ( encY, newSeed ) ->
            Ok ( { share | y = encY }, coreToPcg newSeed )

        Err err ->
            Err err


decryptShare : String -> SecretSharing.EncryptedShare -> Result String SecretSharing.Share
decryptShare key encShare =
    case AES.decrypt key encShare.y of
        Ok decShare ->
            case BigInt.fromString decShare of
                Just y ->
                    Ok { encShare | y = y }

                Nothing ->
                    Err "Decode failed. The password is probably wrong."

        Err e ->
            Err e
