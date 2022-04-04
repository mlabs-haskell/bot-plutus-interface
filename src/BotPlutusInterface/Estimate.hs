-- | Module contains implementation for `estimateBudget` effect
module BotPlutusInterface.Estimate (
  estimateBudget,
  getMaxBudgets,
  testBudget,
) where

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo))
import BotPlutusInterface.QueryNode qualified as QueryNode
import BotPlutusInterface.Types
    ( PABConfig(pcNetwork),
      TxBudget(TxBudget, overallSpendMax, overallMintMax),
      TxFile(..),
      BudgetEstimationError(..),
      MintingBudget,
      SpendingBudget,
      ApiUnitsMap )
import Cardano.Api qualified as CAPI
import Control.Arrow (ArrowChoice (right), left)
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import System.Directory.Internal.Prelude (getEnv)
import Prelude

{- | Estimate budget of transaction.
 Returns separate budgets for spending and minting.
-}
estimateBudget :: PABConfig -> TxFile -> IO (Either BudgetEstimationError TxBudget)
estimateBudget bapConf txFile = do
  sock <- getEnv "CARDANO_NODE_SOCKET_PATH"
  let debugNodeInf = NodeInfo (pcNetwork bapConf) sock
  txBody <- case txFile of
    Raw rp -> deserialiseRaw rp
    Signed sp -> fmap CAPI.getTxBody <$> deserialiseSigned sp

  budgetRes <-
    either
      (pure . Left)
      (getExUnits debugNodeInf)
      txBody
  return (budgetRes >>= toTxBudgets)

-- | Deserialize transaction body from ".signed" file
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

-- | Deserialize transaction body from ".raw" file
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

-- | Calculate execution units using transaction body
getExUnits ::
  NodeInfo ->
  CAPI.TxBody CAPI.AlonzoEra ->
  IO (Either BudgetEstimationError ApiUnitsMap)
getExUnits nodeInf txBody = do
  sysStart <- QueryNode.querySystemStart nodeInf
  eraHist <- QueryNode.queryEraHistory nodeInf
  pparams <- QueryNode.queryProtocolParams nodeInf
  utxo <- QueryNode.queryOutsByInputs nodeInf capiIns
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

    flattenEvalResult = \case
      Right (Right res) -> Right res
      err -> Left $ toBudgetError err

-- | Get maximum budgets for spending and minting taht will fit any spending input or minting policy.
getMaxBudgets :: TxBudget -> (SpendingBudget, MintingBudget)
getMaxBudgets txb = (overallSpendMax txb, overallMintMax txb)

-- | Convert Cardano.Api response of budget estimation to `TxBudget`
toTxBudgets :: ApiUnitsMap -> Either BudgetEstimationError TxBudget
toTxBudgets bdg =
  left toBudgetError $
    TxBudget bdg exBudgets
      <$> pickMax spends
      <*> pickMax mints
  where
    exBudgets = fmap unitsToBudget <$> bdg
    (spends, mints) = separateExUnits bdg
    pickMax =
      right (foldl' (\l r -> l `takeMax` unitsToBudget r) mempty)
        . sequence
        . Map.elems

    takeMax (ExBudget s1 m1) (ExBudget s2 m2) =
      ExBudget (max s1 s2) (max m1 m2)

-- Helper functions
separateExUnits :: ApiUnitsMap -> (ApiUnitsMap, ApiUnitsMap)
separateExUnits =
  Map.foldlWithKey' f (Map.empty, Map.empty)
  where
    f (spends, mints) sWitn eUnits
      | isSpending sWitn = (,mints) $ Map.insert sWitn eUnits spends
      | isMinting sWitn = (spends,) $ Map.insert sWitn eUnits mints
      | otherwise = (spends, mints)

    isSpending = \case CAPI.ScriptWitnessIndexTxIn _ -> True; _ -> False
    isMinting = \case CAPI.ScriptWitnessIndexMint _ -> True; _ -> False

-- | Cardano to Plutus budget converter
unitsToBudget :: CAPI.ExecutionUnits -> ExBudget
unitsToBudget (CAPI.ExecutionUnits cpu mem) =
  ExBudget (ExCPU $ cast cpu) (ExMemory $ cast mem)
  where
    cast = fromInteger . toInteger

-- | Helper error converter
toBudgetError :: Show e => e -> BudgetEstimationError
toBudgetError = BudgetEstimationError . Text.pack . show

-- | For test mocks
testBudget :: TxBudget
testBudget =
  let someBudget = ExBudget (ExCPU 500000) (ExMemory 2000)
   in TxBudget mempty mempty someBudget someBudget
