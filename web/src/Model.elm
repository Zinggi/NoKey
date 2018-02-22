module Model exposing (..)

import Json.Encode as JE exposing (Value)
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
import Data.RequestPassword
import Data.Notifications as Notifications exposing (Notifications, Notification, ShareRequest, SiteEntry)
import Data.Sync exposing (SyncData, OtherSharedData)
import Data.Storage
import Protocol.Data as Protocol
import Views.PasswordGenerator as PW
import Views.Pairing
import Views.Notifications
import Ports


-- Msg


type Msg
    = AddPassword String
    | SiteNameChanged String
    | SecurityLevelChanged Int
    | NewPasswordRequirements PW.State
    | UserNameChanged String
    | PairDeviceClicked
    | GetTokenClicked
    | UpdatePairing Views.Pairing.State
    | TokenSubmitted
    | RemoveDevice String
    | SetDeviceName String
    | InsertSite String String (Dict String SecretSharing.Share) Int Time
    | RequestPasswordPressed ( String, String ) Bool
    | GrantShareRequest Notifications.Id ShareRequest
    | RejectShareRequest Notifications.Id
    | ResetDevice
    | SendOutAccountsFor String
    | AddSiteEntry { isSignUp : Bool, entry : SiteEntry }
    | DeletePassword ( String, String )
    | UpdateNotifications Views.Notifications.State
    | SaveEntry Notifications.Id SiteEntry
    | DismissNotification Notifications.Id
    | FillForm { login : String, site : String, password : String }
    | ProtocolMsg Protocol.Msg
    | OnStateRequest



-- Model


type alias Model =
    { newSiteEntry : PasswordMetaData
    , expandSiteEntry : Bool
    , requirementsState : PW.State
    , pairingDialogue : Views.Pairing.State
    , showPairingDialogue : Bool
    , seed : RandomE.Seed
    , sitesState : Data.RequestPassword.State
    , notifications : Notifications
    , notificationsView : Views.Notifications.State
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
    }


init : Flags -> Model
init { initialSeed, storedState } =
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

        makeInit mayId maySync =
            { newSiteEntry = Data.PasswordMeta.default
            , expandSiteEntry = False
            , requirementsState = PW.init (RandomE.initialSeed base ext)
            , seed = RandomE.initialSeed base ext
            , uniqueIdentifyier = Maybe.withDefault uuid mayId
            , syncData = Maybe.withDefault (Data.Sync.init indepSeed uuid) maySync
            , pairingDialogue = Views.Pairing.init
            , showPairingDialogue = True
            , notifications = Notifications.init
            , notificationsView = Views.Notifications.init
            , sitesState = Data.RequestPassword.init
            , protocolState = Protocol.init
            , currentSite = Nothing
            }
    in
        case Data.Storage.decode storedState of
            Ok { syncData, uniqueIdentifyier } ->
                makeInit (Just uniqueIdentifyier) (Just syncData)

            Err err ->
                let
                    _ =
                        Debug.log "couldn't decode state" err
                in
                    makeInit Nothing Nothing


reset : Model -> Model
reset model =
    let
        int32 =
            (RandomE.int RandomE.minInt RandomE.maxInt)

        ( initSeed, _ ) =
            RandomE.step (RandomE.map2 (,) int32 (RandomE.list 8 int32)) model.seed
    in
        init { initialSeed = initSeed, storedState = JE.null }


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



-- Encoder/Decoder


decode : Value -> Result String Model
decode value =
    Ok (PortUtils.fromJs value)


encode : Model -> Value
encode model =
    PortUtils.toJs model


encodeMsg : Msg -> Value
encodeMsg msg =
    PortUtils.toJs msg


decodeMsg : Value -> Msg
decodeMsg value =
    PortUtils.fromJs value
