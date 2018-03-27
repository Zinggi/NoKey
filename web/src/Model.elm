module Model exposing (..)

import Json.Encode as JE exposing (Value)
import Json.Decode as JD
import Time exposing (Time)
import Dict exposing (Dict)


--

import Random.Pcg as Random exposing (Generator, Seed)
import Random.Pcg.Extended as RandomE
import SecretSharing
import PortUtils


--

import Helper exposing (..)
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications exposing (Notifications, Notification, ShareRequest, SiteEntry)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data exposing (GroupId, AccountId, Password)
import Data.Storage
import Data exposing (..)
import Protocol.Data as Protocol
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Notifications
import Views.Passwords
import Ports


-- Msg


type Msg
    = AddPassword String String
    | SiteNameChanged String
    | SecurityLevelChanged Int
    | NewPasswordRequirements PW.State
    | UserNameChanged String
    | PairDeviceClicked
    | GetTokenClicked
    | UpdatePairing Views.Pairing.State
    | TokenSubmitted
    | DoTokenSubmitted Time
    | RemoveDevice String
    | SetDeviceName String
    | InsertSite AccountId GroupId Password Time
    | RequestPasswordPressed GroupId (Maybe AccountId)
    | GrantShareRequest Notifications.Id ShareRequest
    | RejectShareRequest Notifications.Id
    | ResetDevice
    | SendOutAccountsFor String
    | AddSiteEntry { isSignUp : Bool, entry : SiteEntry }
    | DeletePassword AccountId
    | TogglePassword AccountId
    | UpdateNotifications Views.Notifications.State
    | UpdatePasswordView Views.Passwords.Msg
    | SaveEntry Notifications.Id String SiteEntry
    | DismissNotification Notifications.Id
    | FillForm AccountId
    | ProtocolMsg Protocol.Msg
    | ReceiveMyShares (List ( GroupId, Value ))
    | NewEncryptedShares { time : Time, groupId : GroupId, shares : List ( DeviceId, Value ) }
    | OnStateRequest



-- Model
-- TODO: create other top level type that represents Loading | Loaded Model | Error
-- remember to store state on first loaded, e.g.
--      |> withCmds [ storeState newModel, Api.askForNewVersion newModel.syncData ]
-- This is for both showing decode errors to user and on startup we need to generate two RSA keys, which takes some time


type ModelState
    = Loaded Model
      -- TODO: handle in UI, e.g. show decode error, and offer state backup
    | LoadingError String


type alias Model =
    { newSiteEntry : PasswordMetaData
    , expandSiteEntry : Bool
    , requirementsState : PW.State
    , pairingDialogue : Views.Pairing.State
    , showPairingDialogue : Bool
    , seed : RandomE.Seed
    , notifications : Notifications
    , notificationsView : Views.Notifications.State
    , passwordsView : Views.Passwords.State
    , protocolState : Protocol.State

    -- Keep the current site, to provide site specific actions
    , currentSite : Maybe String

    -- These ones should be serialized:
    , uniqueIdentifyier : String

    -- CRDT for synchronisation
    , syncData : SyncData
    }


type alias Flags =
    { initialSeed : ( Int, List Int )
    , storedState : Value
    , encryptionKey : Value
    , signingKey : Value
    , deviceType : Value
    }


init : Flags -> ModelState
init ({ initialSeed, storedState, encryptionKey, signingKey, deviceType } as flags) =
    case ( Data.Storage.decode storedState, JD.decodeValue Data.Sync.deviceTypeDecoder deviceType ) of
        ( Ok { syncData, uniqueIdentifyier }, Ok devType ) ->
            -- TODO: here we should have the keys ready and store them
            Loaded <| initModel initialSeed encryptionKey signingKey devType (Just uniqueIdentifyier) (Just syncData)

        ( Err "Expecting an object with a field named `syncData` but instead got: null", Ok devType ) ->
            -- This happens when we reset or when used the first time. It's ok.
            Loaded <| initModel initialSeed encryptionKey signingKey devType Nothing Nothing

        ( err, devType ) ->
            LoadingError ("couldn't decode state or deviceType:\n\n" ++ toString err ++ "\n\nDevice type:" ++ toString devType)


initModel initialSeed encryptionKey signingKey devType mayId maySync =
    let
        ( base, ext ) =
            initialSeed

        ( uuid, seed2 ) =
            -- TODO: replace with pcg-extended
            -- adapt the UUID package to use pcg-extended
            -- see:
            --  https://github.com/danyx23/elm-uuid/issues/10
            Random.step randomUUID (Random.initialSeed base)

        ( indepSeed, seed3 ) =
            Random.step Random.independentSeed seed2
    in
        { newSiteEntry = Data.PasswordMeta.default
        , expandSiteEntry = False
        , requirementsState = PW.init (RandomE.initialSeed base ext)
        , seed = RandomE.initialSeed base ext
        , uniqueIdentifyier = Maybe.withDefault uuid mayId
        , syncData = Maybe.withDefault (Data.Sync.init indepSeed encryptionKey signingKey devType uuid) maySync
        , pairingDialogue = Views.Pairing.init
        , showPairingDialogue = True
        , notifications = Notifications.init
        , notificationsView = Views.Notifications.init
        , passwordsView = Views.Passwords.init
        , protocolState = Protocol.init
        , currentSite = Nothing
        }


reset : Model -> Model
reset model =
    let
        int32 =
            (RandomE.int RandomE.minInt RandomE.maxInt)

        ( initSeed, _ ) =
            RandomE.step (RandomE.map2 (,) int32 (RandomE.list 8 int32)) model.seed
    in
        initModel initSeed model.syncData.encryptionKey model.syncData.signingKey model.syncData.deviceType Nothing Nothing


updateNotifications : (Notifications -> Notifications) -> Model -> ( Model, Cmd Msg )
updateNotifications f model =
    let
        newNot =
            f model.notifications
    in
        { model | notifications = newNot }
            |> withCmds [ Ports.notificationCount (Notifications.count newNot) ]


updateProtocol : (Protocol.State -> Protocol.State) -> Model -> Model
updateProtocol f model =
    { model | protocolState = f model.protocolState }


protocolMsg : Protocol.Msg -> Msg
protocolMsg =
    ProtocolMsg


updatePairingDialogue : (Views.Pairing.State -> Views.Pairing.State) -> Model -> Model
updatePairingDialogue f model =
    { model | pairingDialogue = f model.pairingDialogue }



-- Encoder/Decoder


decode : Value -> Result String ModelState
decode value =
    Ok (PortUtils.fromJs value)


encode : ModelState -> Value
encode model =
    PortUtils.toJs model


encodeMsg : Msg -> Value
encodeMsg msg =
    PortUtils.toJs msg


decodeMsg : Value -> Msg
decodeMsg value =
    PortUtils.fromJs value
