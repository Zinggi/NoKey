module Data.KeyBox exposing (KeyBoxId, KeyBox, KeyBoxes, init, merge, decoder, encode)

import Time exposing (Time)
import Random.Pcg as Random exposing (Seed)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE exposing (Value)
import Json.Encode.Extra as JE
import Set
import Dict exposing (Dict)
import Crdt.ORDict as ORDict exposing (ORDict)
import Crdt.TimestampedVersionRegister as TimestampedVersionRegister exposing (TimestampedVersionRegister)
import SecretSharing exposing (EncryptedShare)
import Data exposing (GroupId, DeviceId, encodeGroupId, groupIdDecoder)
import Helper exposing (encodeTuple2, decodeTuple2)
import AES


-- TODO:
-- Static:
--      Verify a password - ok
--
-- Stateful:
--      Create a new box (boxes are created in the 'open' state) - ok
--      Add a share (shares are only added if the box is open) - ok
--      Open a box - ok
--      Close a box - ok
--      Close all boxes - ok
--      Get a share (only works if the box is open) - ok
--      Get shares - ok
--
--  For the password hash:
--  - https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey
--  - https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey
--  - https://nakedsecurity.sophos.com/2013/11/20/serious-security-how-to-store-your-users-passwords-safely/
--
--


type alias KeyBoxes =
    { data : ORDict KeyBoxId KeyBox

    --
    -- Stateful
    --
    -- This keeps the hashed password around.
    -- This will then be used to decrypt secret shares on demand
    , openBoxes : Dict KeyBoxId { key : String }

    -- The seed is used to generate unique ids for a new box
    , seed : Seed
    }


type alias KeyBoxId =
    -- The first one is the id of the device that created the box, the next one is a random, unique int
    ( String, Int )


type alias KeyBox =
    { encryptedShares : ORDict GroupId (TimestampedVersionRegister EncryptedShare)
    , name : TimestampedVersionRegister String

    -- Static. When merging, the second one wins, as these parameters shouldn't change.
    , salt : String

    -- The hashAlgorithm might change in a future version,
    -- but then we can just create a new box to make use of the new algorithm and
    --introduce that field then.
    -- , hashAlgorithm : String
    --
    -- To check if the entered password was correct, we save a hash here. This hash
    -- has to be calculated using the web crypto api. It is crucial that we use a different
    -- seed for this hash, otherwise it would be trivial to open the box without the password!
    , passwordHash : String
    , hashSalt : String
    }


init : Seed -> KeyBoxes
init seed =
    { data = ORDict.init seed, seed = seed, openBoxes = Dict.empty }


isKeyCorrect : KeyBoxId -> String -> KeyBoxes -> Bool
isKeyCorrect id passwordHash boxes =
    case Dict.get id (ORDict.get boxes.data) of
        Just box ->
            box.passwordHash == passwordHash

        Nothing ->
            False


{-| store a share. also rememeber the stored share as long as the box stays open.
-}
storeShare : DeviceId -> Time -> KeyBoxId -> GroupId -> SecretSharing.Share -> KeyBoxes -> KeyBoxes
storeShare deviceId time id groupId share boxes =
    case Dict.get id boxes.openBoxes of
        Just { key } ->
            case AES.encryptShare boxes.seed key share of
                Ok ( encShare, seed ) ->
                    let
                        new =
                            TimestampedVersionRegister.init deviceId time encShare

                        up reg =
                            TimestampedVersionRegister.set deviceId time encShare reg

                        update box =
                            { box | encryptedShares = ORDict.updateOrInsert groupId up new box.encryptedShares }
                    in
                        { boxes
                            | seed = seed
                            , data = ORDict.update id update boxes.data
                            , openBoxes = Dict.insert id { key = key } boxes.openBoxes
                        }

                Err e ->
                    -- Lets just ignore errors
                    Debug.log "something went wrong when storing a key" e
                        |> always boxes

        Nothing ->
            Debug.log "box doesn't exist" id
                |> always boxes


openBox : KeyBoxId -> { key : String, passwordHash : String } -> KeyBoxes -> Result String KeyBoxes
openBox id { key, passwordHash } boxes =
    if isKeyCorrect id passwordHash boxes then
        Ok { boxes | openBoxes = Dict.insert id { key = key } boxes.openBoxes }
    else
        Err "Wrong Password"


closeAllBoxes : KeyBoxes -> KeyBoxes
closeAllBoxes boxes =
    { boxes | openBoxes = Dict.empty }


closeBox : KeyBoxId -> KeyBoxes -> KeyBoxes
closeBox id boxes =
    { boxes | openBoxes = Dict.remove id boxes.openBoxes }


{-| Attempt to get a share. Only works if the box is open
-}
getShareFromBox : KeyBoxId -> GroupId -> KeyBoxes -> Result String SecretSharing.Share
getShareFromBox id groupId boxes =
    case Dict.get id boxes.openBoxes of
        Just { key } ->
            case Dict.get id (ORDict.get boxes.data) of
                Just box ->
                    case Dict.get groupId (ORDict.get box.encryptedShares) of
                        Just encShare ->
                            AES.decryptShare key (TimestampedVersionRegister.get encShare)

                        Nothing ->
                            Err "No key saved with this groupId"

                Nothing ->
                    Err "Box doesn't exist"

        Nothing ->
            Err "Box isn't open"


getShares : GroupId -> KeyBoxes -> List SecretSharing.Share
getShares groupId boxes =
    Dict.foldl
        (\id { key } acc ->
            case getShareFromBox id groupId boxes of
                Ok share ->
                    share :: acc

                Err e ->
                    acc
        )
        []
        boxes.openBoxes


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


{-| Create a new box.
The parameters key, salt, passwordHash and hashSalt are critical.
They should be generated via the web crypto api:

    * key : deriveKey userPassword salt -> key
    * salt : the salt used to derive the key
    * passwordHash : deriveKey userPassword hashSalt -> passwordHash
    * hashSalt : the salt used to derive the passwordHash

The deriveKey function used above should ideally be argon2, but unfortunately
the web crypto api doesn't offer this. So PBKDF2 is hopefully good enough.

-}
createBox : { creatorId : DeviceId, name : String, key : String, salt : String, passwordHash : String, hashSalt : String } -> Time -> KeyBoxes -> KeyBoxes
createBox { creatorId, name, key, salt, passwordHash, hashSalt } time boxes =
    let
        ( i, seed ) =
            getUniqueIdForCreatorId creatorId boxes

        boxId =
            ( creatorId, i )
    in
        { data =
            ORDict.insert boxId
                { salt = salt
                , name = TimestampedVersionRegister.init creatorId time name
                , encryptedShares = ORDict.init seed
                , passwordHash = passwordHash
                , hashSalt = hashSalt
                }
                boxes.data
        , seed = seed
        , openBoxes = Dict.insert boxId { key = key } boxes.openBoxes
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
    { my | encryptedShares = ORDict.merge TimestampedVersionRegister.merge other.encryptedShares my.encryptedShares }


encode : KeyBoxes -> Value
encode boxes =
    ORDict.encode2 encodeKeyBoxId encodeBox boxes.data


encodeBox : KeyBox -> Value
encodeBox box =
    JE.object
        [ ( "encryptedShares"
          , ORDict.encode2 encodeGroupId
                (TimestampedVersionRegister.encode SecretSharing.encodeEncryptedShare)
                box.encryptedShares
          )
        , ( "salt", JE.string box.salt )
        , ( "name", TimestampedVersionRegister.encode JE.string box.name )
        , ( "hashSalt", JE.string box.hashSalt )
        , ( "passwordHash", JE.string box.passwordHash )

        -- , ( "hashAlgorithm", JE.string box.hashAlgorithm )
        ]


decoder : Seed -> Decoder KeyBoxes
decoder seed =
    JD.map
        (\data ->
            { data = data
            , seed = seed
            , openBoxes = Dict.empty
            }
        )
        (ORDict.decoder2 keyBoxIdDecoder (boxDecoder seed) seed)


boxDecoder : Seed -> Decoder KeyBox
boxDecoder seed =
    decode
        (\encS s pwh n hs -> { encryptedShares = encS, salt = s, name = n, passwordHash = pwh, hashSalt = hs })
        |> required "encryptedShares"
            (ORDict.decoder2 groupIdDecoder
                (TimestampedVersionRegister.decoder SecretSharing.encryptedShareDecoder)
                seed
            )
        |> required "salt" JD.string
        |> required "passwordHash" JD.string
        -- |> required "hashAlgorithm" JD.string
        |> required "name" (TimestampedVersionRegister.decoder JD.string)
        |> required "hashSalt" JD.string


keyBoxIdDecoder : Decoder KeyBoxId
keyBoxIdDecoder =
    decodeTuple2 JD.string JD.int


encodeKeyBoxId : KeyBoxId -> Value
encodeKeyBoxId id =
    encodeTuple2 JE.string JE.int id
