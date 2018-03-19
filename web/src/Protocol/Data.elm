module Protocol.Data exposing (..)

import Time exposing (Time)
import Json.Encode as JE exposing (Value)
import Http
import Debounce exposing (Debounce)
import RemoteData exposing (WebData, RemoteData(..))
import SecretSharing
import Crdt.VClock as VClock exposing (VClock)
import Timer exposing (Timer)


--

import Data.Sync exposing (SyncData, OtherSharedData)
import Data exposing (GroupId)
import Data.Notifications


type Msg
    = Server ServerMsg
    | Authenticated String Time AuthenticatedMsg
    | Self SelfMsg


type TimerId
    = Pairing
    | CollectShares
    | ShareRequest Data.Notifications.Id


type SelfMsg
    = NoReply
    | DecodeError String
    | JoinedChannel Value
    | NewMsg Value
    | SyncToOthers Debounce.Msg
    | Timer Timer.Msg
    | OnInterval TimerId Time
    | OnFinishTimer TimerId Time


type ServerMsg
    = ReceiveToken Time (WebData String)
    | PairedWith (Result Http.Error String)


type AuthenticatedMsg
    = FinishPairing String Data.Sync.OtherSharedData
    | SyncUpdate OtherSharedData
    | RequestShare GroupId
    | GrantedShareRequest GroupId SecretSharing.Share
    | GotRemoved
    | NeedsUpdate VClock


type alias State =
    { pairingState : PairingState
    , collectShares : CollectSharesState
    , debounce : Debounce ()
    , timer : Timer TimerId
    }


type CollectSharesState
    = Start
    | WaitForShares


type PairingState
    = Init
    | WaitForPaired Time String
    | WaitForFinished Time String String


init : State
init =
    { pairingState = Init
    , collectShares = Start
    , debounce = Debounce.init
    , timer = Timer.init
    }
