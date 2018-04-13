module Model exposing (..)

import Json.Encode exposing (Value)
import Json.Decode as JD
import Time exposing (Time)
import Navigation exposing (Location)
import Toasty


--

import Random.Pcg as Random
import Random.Pcg.Extended as RandomE
import PortUtils


--

import Helper exposing (withCmds)
import Data.PasswordMeta exposing (PasswordMetaData)
import Data.Notifications as Notifications exposing (Notifications, ShareRequest, SiteEntry)
import Data.Sync exposing (SyncData)
import Data exposing (GroupId, AccountId, Password, DeviceId)
import Data.Storage
import Protocol.Data as Protocol
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Notifications
import Views.Passwords
import Ports
import Route exposing (Page)


-- Msg


type Msg
    = AddPassword String String
    | SiteNameChanged String
    | SecurityLevelChanged Int
    | NewPasswordRequirements PW.State
    | UserNameChanged String
    | GetTokenClicked
    | UpdatePairing Views.Pairing.State
    | TokenSubmitted
    | DoTokenSubmitted Time
    | RemoveDevice String
    | SetDeviceName String
    | InsertSite AccountId GroupId Password Time
    | RequestPasswordPressed (List GroupId) (Maybe AccountId)
    | GrantShareRequest Notifications.Id ShareRequest
    | RejectShareRequest Notifications.Id ShareRequest
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
    | SharesReadyToSend { deviceId : DeviceId, encryptedShares : Value, reqIds : Value }
    | DidDecryptRequestedShares { shares : Value, time : Time, otherId : DeviceId, ids : List String }
    | OnStateRequest
    | NavigateTo Page
    | SetPage Page
    | DoneWithTutorial
    | NavigateBack
    | ToastyMsg (Toasty.Msg String)
    | ShowToast String



-- Model


type ModelState
    = Loaded Model
      -- TODO: handle in UI, e.g. show decode error, and offer help, e.g. state backup
    | LoadingError String


type alias Toast =
    Toasty.Stack String


type alias Model =
    { newSiteEntry : PasswordMetaData
    , requirementsState : PW.State
    , pairingDialogue : Views.Pairing.State
    , seed : RandomE.Seed
    , notifications : Notifications
    , notificationsView : Views.Notifications.State
    , passwordsView : Views.Passwords.State
    , protocolState : Protocol.State
    , currentPage : Page
    , toasties : Toast

    -- Keep the current site, to provide site specific actions
    , currentSite : Maybe String

    -- These ones should be serialized:
    , uniqueIdentifyier : String
    , isFirstTimeUser : Bool

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


init : Flags -> Location -> ModelState
init { initialSeed, storedState, encryptionKey, signingKey, deviceType } location =
    case ( Data.Storage.decode storedState, JD.decodeValue Data.Sync.deviceTypeDecoder deviceType ) of
        ( Ok { syncData, uniqueIdentifyier, isFirstTimeUser }, Ok devType ) ->
            Loaded <| initModel isFirstTimeUser (Just location) initialSeed encryptionKey signingKey devType (Just uniqueIdentifyier) (Just syncData)

        ( Err "Expecting an object with a field named `syncData` but instead got: null", Ok devType ) ->
            -- This happens when we reset or when used the first time. It's ok.
            Loaded <| initModel True (Just location) initialSeed encryptionKey signingKey devType Nothing Nothing

        ( Err "Expecting an object with a field named `uuid` but instead got: null", Ok devType ) ->
            -- This happens when we reset or when used the first time. It's ok.
            Loaded <| initModel True (Just location) initialSeed encryptionKey signingKey devType Nothing Nothing

        ( err, devType ) ->
            LoadingError ("couldn't decode state or deviceType:\n\n" ++ toString err ++ "\n\nDevice type:" ++ toString devType)


initModel isFirstTimeUser location initialSeed encryptionKey signingKey devType mayId maySync =
    let
        ( base, ext ) =
            initialSeed

        ( uuid, newSeed ) =
            RandomE.step Helper.randomUUID (RandomE.initialSeed base ext)

        ( indepSeed, _ ) =
            Random.step Random.independentSeed (Random.initialSeed base)
    in
        { newSiteEntry = Data.PasswordMeta.default
        , requirementsState = PW.init (RandomE.initialSeed base ext)
        , seed = newSeed
        , uniqueIdentifyier = Maybe.withDefault uuid mayId
        , syncData = Maybe.withDefault (Data.Sync.init indepSeed encryptionKey signingKey devType uuid) maySync
        , pairingDialogue = Views.Pairing.init
        , notifications = Notifications.init
        , notificationsView = Views.Notifications.init
        , passwordsView = Views.Passwords.init
        , protocolState = Protocol.init
        , currentSite = Nothing
        , isFirstTimeUser = isFirstTimeUser
        , currentPage = Maybe.map Route.fromLocation location |> Maybe.withDefault Route.Home
        , toasties = Toasty.initialState
        }


getUniqueId : Model -> ( String, Model )
getUniqueId model =
    let
        ( uuid, newSeed ) =
            RandomE.step Helper.randomUUID model.seed
    in
        ( uuid, { model | seed = newSeed } )


reset : Model -> Model
reset model =
    let
        int32 =
            RandomE.int RandomE.minInt RandomE.maxInt

        ( initSeed, _ ) =
            RandomE.step (RandomE.map2 (,) int32 (RandomE.list 8 int32)) model.seed
    in
        initModel model.isFirstTimeUser Nothing initSeed model.syncData.encryptionKey model.syncData.signingKey model.syncData.deviceType Nothing Nothing


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
