module Data.KeyBox exposing (KeyBoxId, KeyBox, KeyBoxes, init, merge, decoder, encode)

import Random.Pcg as Random exposing (Seed)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import SecretSharing exposing (EncryptedShare)
import Data exposing (GroupId, encodeGroupId, groupIdDecoder)
import Helper exposing (encodeTuple2, decodeTuple2)


-- TODO:
-- Static:
--      Create a new box
--      Add a key
--      Verify a password
--
-- Stateful:
--      Create a new box (boxes are created in the 'open' state)
--      Open a box
--      Close a box
--


type alias KeyBoxes =
    ORDict KeyBoxId KeyBox


type alias KeyBoxId =
    -- The first one is the id of the device that created the box, the next one is a random, unique int
    ( String, Int )


type alias KeyBox =
    { encrypedShares : ORDict GroupId (TimestampedVersionRegister EncryptedShare)

    -- Static. When merging the second one wins, as these parameters shouldn't change.
    -- The hashAlgorithm might change in a future version,
    -- but then we can just create a new box to make use of the new algorithm.
    , salt : String
    , passwordHash : String
    , hashAlgorithm : String
    }


init : Seed -> KeyBoxes
init seed =
    ORDict.init seed


merge : KeyBoxes -> KeyBoxes -> KeyBoxes
merge other my =
    ORDict.merge mergeBox other my


mergeBox : KeyBox -> KeyBox -> KeyBox
mergeBox other my =
    { my | encrypedShares = ORDict.merge TimestampedVersionRegister.merge other.encrypedShares my.encrypedShares }


encode : KeyBoxes -> Value
encode boxes =
    ORDict.encode2 encodeKeyBoxId encodeBox boxes


encodeBox : KeyBox -> Value
encodeBox box =
    JE.object
        [ ( "encrypedShares"
          , ORDict.encode2 encodeGroupId
                (TimestampedVersionRegister.encode SecretSharing.encodeEncryptedShare)
                box.encrypedShares
          )
        , ( "salt", JE.string box.salt )
        , ( "passwordHash", JE.string box.passwordHash )
        , ( "hashAlgorithm", JE.string box.hashAlgorithm )
        ]


decoder : Decoder KeyBoxes
decoder =
    ORDict.decoder2 keyBoxIdDecoder boxDecoder


boxDecoder : Decoder KeyBox
boxDecoder =
    decode (\encS s p h -> { encrypedShares = encS, salt = s, passwordHash = p, hashAlgorithm = h })
        |> required "encrypedShares"
            (ORDict.decoder2 groupIdDecoder
                (TimestampedVersionRegister.decoder SecretSharing.encryptedShareDecoder)
            )
        |> required "salt" JD.string
        |> required "passwordHash" JD.string
        |> required "hashAlgorithm" JD.string


keyBoxIdDecoder : Decoder KeyBoxId
keyBoxIdDecoder =
    decodeTuple2 JD.string JD.int


encodeKeyBoxId : KeyBoxId -> Value
encodeKeyBoxId id =
    encodeTuple2 JE.string JE.int id
