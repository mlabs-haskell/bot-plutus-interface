{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.Types (
  PABConfig (..),
  CLILocation (..),
  AppState (AppState),
  LogLevel (..),
  ContractEnvironment (..),
  ContractState (..),
  SomeContractState (SomeContractState),
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (TVar)
import Data.Aeson (ToJSON)
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import Ledger (PubKeyHash)
import Network.Wai.Handler.Warp (Port)
import Plutus.PAB.Core.ContractInstance.STM (Activity)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Wallet.Emulator (Wallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

data PABConfig = PABConfig
  { -- | Calling the cli through ssh when set to Remote
    pcCliLocation :: !CLILocation
  , pcChainIndexUrl :: !BaseUrl
  , pcNetwork :: !NetworkId
  , pcProtocolParams :: !ProtocolParameters
  , -- | Directory name of the script and data files
    pcScriptFileDir :: !Text
  , -- | Directory name of the signing key files
    pcSigningKeyFileDir :: !Text
  , -- | Directory name of the transaction files
    pcTxFileDir :: !Text
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    pcProtocolParamsFile :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    pcDryRun :: !Bool
  , pcLogLevel :: !LogLevel
  , pcOwnPubKeyHash :: PubKeyHash
  , pcPort :: !Port
  }
  deriving stock (Show, Eq)

data ContractEnvironment w = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceContractState :: TVar (ContractState w)
  , ceWallet :: Wallet
  }
  deriving stock (Show)

instance Show (TVar (ContractState w)) where
  show _ = "<ContractState>"

newtype AppState = AppState (TVar (Map ContractInstanceId SomeContractState))

{- | This type is wrapping a ContractState in a TVar and existentially quantifying the @w@
 type variable, so we can store different contracts in the AppState
-}
data SomeContractState
  = forall (w :: Type). (ToJSON w) => SomeContractState (TVar (ContractState w))

data ContractState w = ContractState
  { csActivity :: Activity
  , csObservableState :: w
  }
  deriving stock (Show, Eq)

data CLILocation = Local | Remote Text
  deriving stock (Show, Eq)

data LogLevel = Error | Warn | Notice | Info | Debug
  deriving stock (Eq, Ord, Show)

instance Default PABConfig where
  def =
    PABConfig
      { pcCliLocation = Local
      , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
      , pcNetwork = Testnet (NetworkMagic 42)
      , pcProtocolParams = def
      , pcScriptFileDir = "./result-scripts"
      , pcSigningKeyFileDir = "./signing-keys"
      , pcTxFileDir = "./txs"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      , pcLogLevel = Info
      , pcOwnPubKeyHash = ""
      , pcPort = 9080
      }
