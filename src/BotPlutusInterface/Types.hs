{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BotPlutusInterface.Types (
  PABConfig (..),
  CLILocation (..),
  AppState (AppState),
  LogLevel (..),
  ContractEnvironment (..),
  Tip (Tip, epoch, hash, slot, block, era, syncProgress),
  ContractState (..),
  SomeContractState (SomeContractState),
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
  RawTx (..),
  TxFile (..),
  TxBudget (..),
  BudgetEstimationError (..),
  SpendBudgets,
  MintBudgets,
  ContractStats (..),
  addBudget,
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..), ScriptExecutionError, ScriptWitnessIndex)
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (TVar)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (
  ExBudget,
  MintingPolicyHash,
  PubKeyHash,
  StakePubKeyHash,
  TxId,
  TxOutRef,
 )
import Ledger.TimeSlot (SlotConfig)
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import Plutus.PAB.Core.ContractInstance.STM (Activity)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Wallet.Types (ContractInstanceId (..))
import Prelude

data PABConfig = PABConfig
  { -- | Calling the cli through ssh when set to Remote
    pcCliLocation :: !CLILocation
  , pcChainIndexUrl :: !BaseUrl
  , pcNetwork :: !NetworkId
  , pcProtocolParams :: !ProtocolParameters
  , -- | Slot configuration of the network, the default value can be used for the mainnet
    pcSlotConfig :: !SlotConfig
  , -- | Directory name of the script and data files
    pcScriptFileDir :: !Text
  , -- | Directory name of the signing key files
    pcSigningKeyFileDir :: !Text
  , -- | Directory name of the transaction files
    pcTxFileDir :: !Text
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    pcProtocolParamsFile :: !Text
  , -- | Directory name of metadata files
    pcMetadataDir :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    pcDryRun :: !Bool
  , pcLogLevel :: !LogLevel
  , pcOwnPubKeyHash :: !PubKeyHash
  , pcOwnStakePubKeyHash :: !(Maybe StakePubKeyHash)
  , pcTipPollingInterval :: !Natural
  , pcPort :: !Port
  , pcEnableTxEndpoint :: !Bool
  , pcCollectStats :: !Bool
  }
  deriving stock (Show, Eq)

-- Budget estimation types

{- | Error returned in case any error happened during budget estimation
 (wraps whatever received in `Text`)
-}
data BudgetEstimationError
  = -- | general error for `Cardano.Api` errors
    BudgetEstimationError !Text
  | -- | script evaluation failed during budget estimation
    ScriptFailure ScriptExecutionError
  | {- budget for input or policy was not found after estimation
       (arguably should not happen at all) -}
    BudgetNotFound ScriptWitnessIndex
  deriving stock (Show)

-- | Type of transaction file used for budget estimation
data TxFile
  = -- | for using with ".raw" files
    Raw !FilePath
  | -- | for using with ".signed" files
    Signed !FilePath

-- | Result of budget estimation
data TxBudget = TxBudget
  { -- | budgets for spending inputs
    spendBudgets :: !SpendBudgets
  , -- | budgets for minting policies
    mintBudgets :: !MintBudgets
  }
  deriving stock (Show)

addBudget :: TxId -> TxBudget -> ContractStats -> ContractStats
addBudget txId budget stats =
  stats {estimatedBudgets = Map.insert txId budget (estimatedBudgets stats)}

instance Semigroup TxBudget where
  TxBudget s m <> TxBudget s' m' = TxBudget (s <> s') (m <> m')

instance Monoid TxBudget where
  mempty = TxBudget mempty mempty

type SpendBudgets = Map TxOutRef ExBudget

type MintBudgets = Map MintingPolicyHash ExBudget

{- | Collection of stats that could be collected py `bpi`
   about contract it runs
-}
newtype ContractStats = ContractStats
  { estimatedBudgets :: Map TxId TxBudget
  }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance Show (TVar ContractStats) where
  show _ = "<ContractStats>"

data ContractEnvironment w = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceContractState :: TVar (ContractState w)
  , ceContractStats :: TVar ContractStats
  }
  deriving stock (Show)

data Tip = Tip
  { epoch :: Integer
  , hash :: Text
  , slot :: Integer
  , block :: Integer
  , era :: Text
  , syncProgress :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.FromJSON)

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
      , pcSlotConfig = def
      , pcTipPollingInterval = 10_000_000
      , pcScriptFileDir = "./result-scripts"
      , pcSigningKeyFileDir = "./signing-keys"
      , pcTxFileDir = "./txs"
      , pcMetadataDir = "/metadata"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      , pcLogLevel = Info
      , pcOwnPubKeyHash = ""
      , pcOwnStakePubKeyHash = Nothing
      , pcPort = 9080
      , pcEnableTxEndpoint = False
      , pcCollectStats = False
      }

data RawTx = RawTx
  { _type :: Text
  , _description :: Text
  , _cborHex :: Text
  }
  deriving (Generic, Eq, Show)

-- type is a reserved keyword in haskell and can not be used as a field name
-- when converting this to JSON we drop the _ prefix from each field
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''RawTx
