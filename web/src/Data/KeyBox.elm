module Data.KeyBox exposing (KeyBoxId, KeyBox, KeyBoxes, init, merge, decoder, encode)

import Time exposing (Time)
import Random.Pcg as Random exposing (Seed)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Set
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import SecretSharing exposing (EncryptedShare)
import Data exposing (GroupId, DeviceId, encodeGroupId, groupIdDecoder)
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
--  For the password hash:
--  - https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey
--  - https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey
--  - https://nakedsecurity.sophos.com/2013/11/20/serious-security-how-to-store-your-users-passwords-safely/
--
--


type alias KeyBoxes =
    { data : ORDict KeyBoxId KeyBox
    , seed : Seed
    }


type alias KeyBoxId =
    -- The first one is the id of the device that created the box, the next one is a random, unique int
    ( String, Int )


type alias KeyBox =
    { encrypedShares : ORDict GroupId (TimestampedVersionRegister EncryptedShare)
    , name : TimestampedVersionRegister String

    -- Static. When merging, the second one wins, as these parameters shouldn't change.
    , salt : String

    -- The hashAlgorithm might change in a future version,
    -- but then we can just create a new box to make use of the new algorithm and
    --introduce that field then.
    -- , hashAlgorithm : String
    --
    -- To check if the entered password was correct, we could save some hash here,
    -- e.g. argon2 or scrypt. But for now, we check if the password was correct by
    -- trying to parse the encrypted y part as a number. If it succeeds, the password was
    -- probably correct. It is extremely unlikely, that by accidentally misspell the password,
    -- we also get a valid but wrong number. It is far more likely that it will decrypt
    -- to some random string of characters that isn't a number.
    -- , passwordHash : String
    }


init : Seed -> KeyBoxes
init seed =
    { data = ORDict.init seed, seed = seed }


getUniqueIdForCreatorId : DeviceId -> KeyBoxes -> ( Int, Seed )
getUniqueIdForCreatorId devId boxes =
    let
        ( i, seed ) =
            Random.step (Random.int Random.minInt Random.maxInt) boxes.seed

        keys =
            ORDict.keys boxes.data
                |> Set.filter (\( id, n ) -> devId == id)
                |> Set.foldl (\( id, n ) acc -> Set.insert n acc) Set.empty
    in
        if Set.member i keys then
            -- try again
            getUniqueIdForCreatorId devId { boxes | seed = seed }
        else
            ( i, seed )


createBox : { creatorId : DeviceId, name : String, hashedPassword : String, salt : String } -> Time -> KeyBoxes -> KeyBoxes
createBox { creatorId, name, hashedPassword, salt } time boxes =
    let
        ( i, seed ) =
            getUniqueIdForCreatorId creatorId boxes
    in
        { data =
            ORDict.insert ( creatorId, i )
                { salt = salt, name = TimestampedVersionRegister.init creatorId time name, encrypedShares = ORDict.init seed }
                boxes.data
        , seed = seed
        }


{-| merge two key boxes. WARNING: the order of arguments matters,
the first is merged into the second, e.g:

    merge other my -> newMy

-}
merge : KeyBoxes -> KeyBoxes -> KeyBoxes
merge other my =
    { my | data = ORDict.merge mergeBox other.data my.data }


mergeBox : KeyBox -> KeyBox -> KeyBox
mergeBox other my =
    { my | encrypedShares = ORDict.merge TimestampedVersionRegister.merge other.encrypedShares my.encrypedShares }


encode : KeyBoxes -> Value
encode boxes =
    ORDict.encode2 encodeKeyBoxId encodeBox boxes.data


encodeBox : KeyBox -> Value
encodeBox box =
    JE.object
        [ ( "encrypedShares"
          , ORDict.encode2 encodeGroupId
                (TimestampedVersionRegister.encode SecretSharing.encodeEncryptedShare)
                box.encrypedShares
          )
        , ( "salt", JE.string box.salt )
        , ( "name", TimestampedVersionRegister.encode JE.string box.name )

        -- , ( "passwordHash", JE.string box.passwordHash )
        -- , ( "hashAlgorithm", JE.string box.hashAlgorithm )
        ]


decoder : Seed -> Decoder KeyBoxes
decoder seed =
    JD.map
        (\data ->
            { data = data
            , seed = seed
            }
        )
        (ORDict.decoder2 keyBoxIdDecoder (boxDecoder seed) seed)


boxDecoder : Seed -> Decoder KeyBox
boxDecoder seed =
    decode
        (\encS s n -> { encrypedShares = encS, salt = s, name = n })
        |> required "encrypedShares"
            (ORDict.decoder2 groupIdDecoder
                (TimestampedVersionRegister.decoder SecretSharing.encryptedShareDecoder)
                seed
            )
        |> required "salt" JD.string
        -- |> required "passwordHash" JD.string
        -- |> required "hashAlgorithm" JD.string
        |> required "name" (TimestampedVersionRegister.decoder JD.string)


keyBoxIdDecoder : Decoder KeyBoxId
keyBoxIdDecoder =
    decodeTuple2 JD.string JD.int


encodeKeyBoxId : KeyBoxId -> Value
encodeKeyBoxId id =
    encodeTuple2 JE.string JE.int id
