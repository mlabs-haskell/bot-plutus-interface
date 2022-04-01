module BotPlutusInterface.Estimate (
  BudgetEstimationError,
  TxBudgets,
  TxFile(..),
  budgetByFile,
  getMaxBudgets,
 ) where

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo))
import BotPlutusInterface.QueryNode qualified as QueryNode
import Cardano.Api qualified as CAPI
import Control.Arrow (ArrowChoice (right), left)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import System.Directory.Internal.Prelude (getEnv)
import Prelude

data BudgetEstimationError = BudgetEstimationError Text
  deriving stock (Show)

data TxFile
  = Raw FilePath
  | Signed FilePath

budgetByFile :: TxFile -> IO (Either BudgetEstimationError TxBudgets)
budgetByFile txFile = do
  sock <- getEnv "CARDANO_NODE_SOCKET_PATH"
  let debugNodeInf = NodeInfo CAPI.Mainnet sock
  txBody <- case txFile of
    Raw rp -> deserialiseRaw rp
    Signed sp -> fmap CAPI.getTxBody <$> deserialiseSigned sp

  budgetRes <-
    either
      (pure . Left)
      (getExUnits debugNodeInf)
      txBody
  return (budgetRes >>= toTxBudgets)

deserialiseSigned :: FilePath -> IO (Either BudgetEstimationError (CAPI.Tx CAPI.AlonzoEra))
deserialiseSigned txFile = do
  envlp <- readEnvelope
  return $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope CAPI.AsAlonzoTx

deserialiseRaw :: FilePath -> IO (Either BudgetEstimationError (CAPI.TxBody CAPI.AlonzoEra))
deserialiseRaw txFile = do
  envlp <- readEnvelope
  return $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope (CAPI.AsTxBody CAPI.AsAlonzoEra)

type ApiUnitsMap = Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError CAPI.ExecutionUnits)

type ExBudgetsMap = Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError ExBudget)

type SpendingBudget = ExBudget
type MintingBudget = ExBudget

data TxBudgets = TxBudgets
  { exUnitsMap :: ApiUnitsMap,
    exBudgetsMap :: ExBudgetsMap,
    overallSpendMax :: SpendingBudget,
    overallMintMax :: MintingBudget
  }
  deriving stock (Show)

getMaxBudgets :: TxBudgets -> (SpendingBudget, MintingBudget)
getMaxBudgets txb = (overallSpendMax txb, overallMintMax txb)

toTxBudgets :: ApiUnitsMap -> Either BudgetEstimationError TxBudgets
toTxBudgets bdg =
  left toBudgetError $
    TxBudgets bdg exBudgets
      <$> pickMax spends
      <*> pickMax mints
  where
    exBudgets = fmap unitsToBudget <$> bdg
    (spends, mints) = divideExUnits bdg
    pickMax =
      right (foldl' (\l r -> l `takeMax` unitsToBudget r) mempty)
        . sequence
        . Map.elems

takeMax :: ExBudget -> ExBudget -> ExBudget
takeMax (ExBudget s1 m1) (ExBudget s2 m2) =
  ExBudget (max s1 s2) (max m1 m2)

divideExUnits :: ApiUnitsMap -> (ApiUnitsMap, ApiUnitsMap)
divideExUnits =
  Map.foldlWithKey' f (Map.empty, Map.empty)
  where
    f (spends, mints) sWitn eUnits
      | isSpending sWitn = (,mints) $ Map.insert sWitn eUnits spends
      | isMinting sWitn = (spends,) $ Map.insert sWitn eUnits mints
      | otherwise = (spends, mints)

    isSpending = \case CAPI.ScriptWitnessIndexTxIn _ -> True; _ -> False
    isMinting = \case CAPI.ScriptWitnessIndexMint _ -> True; _ -> False

unitsToBudget :: CAPI.ExecutionUnits -> ExBudget
unitsToBudget (CAPI.ExecutionUnits cpu mem) =
  ExBudget (ExCPU $ cast cpu) (ExMemory $ cast mem)
  where
    cast = fromInteger . toInteger

getExUnits ::
  NodeInfo ->
  CAPI.TxBody CAPI.AlonzoEra ->
  IO (Either BudgetEstimationError ApiUnitsMap)
getExUnits nodeInf txBody = do
  sysStart <- QueryNode.systemStart nodeInf
  eraHist <- QueryNode.eraHistory nodeInf
  pparams <- QueryNode.protocolParams nodeInf
  utxo <- QueryNode.outsByInputs nodeInf (trace ("CAPI ins: " ++ show capiIns) capiIns) -- TODO: remove tracing
  return $
    flattenEvalResult $
      CAPI.evaluateTransactionExecutionUnits CAPI.AlonzoEraInCardanoMode
        <$> sysStart
        <*> eraHist
        <*> pparams
        <*> utxo
        <*> pure txBody
  where
    capiIns :: [CAPI.TxIn]
    capiIns =
      let (CAPI.TxBody txbc) = txBody
       in fst <$> CAPI.txIns txbc

toBudgetError :: Show e => e -> BudgetEstimationError
toBudgetError = BudgetEstimationError . Text.pack . show

flattenEvalResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either BudgetEstimationError b
flattenEvalResult = \case
  Right (Right res) -> Right res
  err -> Left $ toBudgetError err
