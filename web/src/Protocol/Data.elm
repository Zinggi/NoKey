module Protocol.Data exposing (..)

import Time exposing (Time)
import Set exposing (Set)
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Http
import Debounce exposing (Debounce)
import RemoteData exposing (WebData, RemoteData(..))
import Crdt.VClock exposing (VClock)
import Timer exposing (Timer)


--

import Data.Sync exposing (OtherSharedData)
import Data exposing (GroupId, DeviceId)
import Data.Notifications


type Msg
    = Server ServerMsg
    | Authenticated String Time AuthenticatedMsg
    | FinishPairing String Time String Data.Sync.OtherSharedData
      -- We get this when we receive an authenticated msg.
      -- We first have to check if the data matches the signature and only then will it become an AuthenticatedMsg
      -- The two Value's are data and signature, in that order.
    | Unverified String Time Value Value
    | Self SelfMsg


type TimerId
    = Pairing
    | CollectShares String
    | ShareRequest Data.Notifications.Id


type SelfMsg
    = NoReply
    | SendAuthenticatedMsgTo DeviceId Value Value
    | DecodeError String
    | FailedToVerifyAuthenticityOf String Time Value
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
    = SyncUpdate OtherSharedData
    | RequestShares String (List GroupId)
    | RejectShareRequest (Set String)
    | GrantedShareRequest (Set String) Value {- (List ( GroupId, SecretSharing.Share )) -}
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
    | WaitForShares (Dict String ( Set DeviceId, List GroupId ))


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
