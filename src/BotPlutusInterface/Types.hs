{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BotPlutusInterface.Types (
  PABConfig (..),
  CLILocation (..),
  AppState (AppState),
  LogContext (..),
  LogLevel (..),
  LogType (..),
  LogLine (..),
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
  TxStatusPolling (..),
  LogsList (..),
  CollateralUtxo (..),
  CollateralVar (..),
  addBudget,
  readCollateralUtxo,
  collateralValue,
  sufficientLogLevel,
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..), ScriptExecutionError, ScriptWitnessIndex)
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (TVar, readTVarIO)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.Data (Data (toConstr), constrIndex, dataTypeOf, eqT, fromConstrB, indexConstr, type (:~:) (Refl))
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.List (intersect)
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
import Ledger qualified
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import Plutus.PAB.Core.ContractInstance.STM (Activity)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Plutus.V1.Ledger.Ada qualified as Ada
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as PP
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
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
  , -- | Collect contract execution stats inside ContractEnvironment
    pcCollectStats :: !Bool
  , -- | Collect logs inside ContractEnvironment, doesn't depend on log level
    pcCollectLogs :: !Bool
  , pcBudgetMultiplier :: !Rational
  , pcTxStatusPolling :: !TxStatusPolling
  , -- | User defined size of collateral, in Lovelaces
    pcCollateralSize :: !Natural
  }
  deriving stock (Show, Eq)

collateralValue :: PABConfig -> Ledger.Value
collateralValue = Ada.lovelaceValueOf . toInteger . pcCollateralSize

{- | Settings for `Contract.awaitTxStatusChange` implementation.
 See also `BotPlutusInterface.Contract.awaitTxStatusChange`
-}
data TxStatusPolling = TxStatusPolling
  { -- | Interval between `chain-index` queries, microseconds
    spInterval :: !Natural
  , -- | Number of blocks to wait until timeout.
    --   Timeout is required because transaction can be silently discarded from node mempool
    --   and never appear in `chain-index` even if it was submitted successfully to the node
    --   (chain-sync protocol won't help here also)
    spBlocksTimeOut :: !Natural
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

{- | Single log message
 Defined for pretty instance.
-}
data LogLine = LogLine
  { logLineContext :: LogContext
  , logLineLevel :: LogLevel
  , logLineMsg :: PP.Doc ()
  }
  deriving stock (Show)

instance Pretty LogLine where
  pretty (LogLine msgCtx msgLogLvl msg) = pretty msgCtx <+> pretty msgLogLvl <+> PP.unAnnotate msg

-- | List of logs.
newtype LogsList = LogsList
  { getLogsList :: [LogLine]
  }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance Pretty LogsList where
  pretty = PP.vcat . map pretty . getLogsList

instance Show (TVar LogsList) where
  show _ = "<ContractLogs>"

newtype CollateralUtxo = CollateralUtxo
  { collateralTxOutRef :: TxOutRef
  }
  deriving stock (Show)
  deriving newtype (Eq)

instance Pretty CollateralUtxo where
  pretty (CollateralUtxo txOutRef) = "Collateral" <+> pretty txOutRef

data ContractEnvironment w = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceContractState :: TVar (ContractState w)
  , ceContractStats :: TVar ContractStats
  , ceContractLogs :: TVar LogsList
  , ceCollateral :: CollateralVar
  }
  deriving stock (Show)

newtype CollateralVar = CollateralVar
  { unCollateralVar :: TVar (Maybe CollateralUtxo)
  }
instance Show CollateralVar where
  show _ = "<Collateral TxOutRef>"

readCollateralUtxo :: forall (w :: Type). ContractEnvironment w -> IO (Maybe CollateralUtxo)
readCollateralUtxo = readTVarIO . unCollateralVar . ceCollateral

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

data LogType
  = CoinSelectionLog
  | TxBalancingLog
  | CollateralLog
  | PABLog
  | AnyLog
  deriving stock (Eq, Ord, Show, Data)

instance Pretty LogType where
  pretty CoinSelectionLog = "CoinSelection"
  pretty TxBalancingLog = "TxBalancing"
  pretty CollateralLog = "Collateral"
  pretty PABLog = "PABLog"
  pretty AnyLog = "Any"

data LogLevel
  = Error {ltLogTypes :: [LogType]}
  | Warn {ltLogTypes :: [LogType]}
  | Notice {ltLogTypes :: [LogType]}
  | Info {ltLogTypes :: [LogType]}
  | Debug {ltLogTypes :: [LogType]}
  deriving stock (Eq, Show, Data)

instance Enum LogLevel where
  fromEnum = (\a -> a - 1) . constrIndex . toConstr
  toEnum = fromConstrB field . indexConstr (dataTypeOf $ Notice []) . (+ 1)
    where
      field :: forall a. Data a => a
      field = case eqT :: Maybe (a :~: [LogType]) of
        Just Refl -> [AnyLog]
        Nothing -> error "Expected a value of type LogType."

instance Pretty LogLevel where
  pretty = \case
    Debug a -> "[DEBUG " <> pretty a <> "]"
    Info a -> "[INFO " <> pretty a <> "]"
    Notice a -> "[NOTICE " <> pretty a <> "]"
    Warn a -> "[WARNING " <> pretty a <> "]"
    Error a -> "[ERROR " <> pretty a <> "]"

{- | if sufficientLogLevel settingLogLevel msgLogLvl
 then message should be displayed with this log level setting.
-}
sufficientLogLevel :: LogLevel -> LogLevel -> Bool
sufficientLogLevel logLevelSetting msgLogLvl =
  msgLogLvl `constrLEq` logLevelSetting -- the log is important enough
    && not (null intersectLogTypes) -- log is of type we're interested in
  where
    intersectLogTypes = ltLogTypes logLevelSetting `intersect` (ltLogTypes msgLogLvl <> [AnyLog])

    constrLEq a b = fromEnum a <= fromEnum b

data LogContext = BpiLog | ContractLog
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Pretty LogContext where
  pretty = \case
    BpiLog -> "[BPI]"
    ContractLog -> "[CONTRACT]"

instance Default PABConfig where
  def =
    PABConfig
      { pcCliLocation = Local
      , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
      , pcNetwork = Testnet (NetworkMagic 42)
      , pcProtocolParams = def
      , pcTipPollingInterval = 10_000_000
      , pcScriptFileDir = "./result-scripts"
      , pcSigningKeyFileDir = "./signing-keys"
      , pcTxFileDir = "./txs"
      , pcMetadataDir = "/metadata"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      , pcLogLevel = Info [AnyLog]
      , pcOwnPubKeyHash = ""
      , pcOwnStakePubKeyHash = Nothing
      , pcPort = 9080
      , pcEnableTxEndpoint = False
      , pcCollectStats = False
      , pcCollectLogs = False
      , pcBudgetMultiplier = 1
      , pcTxStatusPolling = TxStatusPolling 1_000_000 8
      , pcCollateralSize = 10_000_000
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
