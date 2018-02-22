module Protocol.Data exposing (..)

import Time exposing (Time)
import Json.Encode as JE exposing (Value)
import Http
import Debounce exposing (Debounce)
import RemoteData exposing (WebData, RemoteData(..))
import SecretSharing
import Crdt.VClock as VClock exposing (VClock)


--

import Data.Sync exposing (SyncData, OtherSharedData)


type Msg
    = Server ServerMsg
    | Authenticated String Time AuthenticatedMsg
    | Self SelfMsg


type SelfMsg
    = NoReply
    | DecodeError String
    | JoinedChannel Value
    | NewMsg Value
    | SyncToOthers Debounce.Msg


type ServerMsg
    = ReceiveToken Time (WebData String)
    | PairedWith (Result Http.Error String)


type AuthenticatedMsg
    = FinishPairing String Data.Sync.OtherSharedData
    | SyncUpdate OtherSharedData
    | RequestShare ( String, String )
    | GrantedShareRequest ( String, String ) SecretSharing.Share
    | GotRemoved
    | NeedsUpdate VClock


type alias State =
    { pairingState : PairingState
    , debounce : Debounce ()
    }


type PairingState
    = Init
    | WaitForPaired Time String
      -- TODO: Why keep otherId here? Is it to resend?
    | WaitForFinished Time String String


init : State
init =
    { pairingState = Init
    , debounce = Debounce.init
    }
