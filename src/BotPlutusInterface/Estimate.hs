-- | Module contains implementation for `estimateBudget` effect
module BotPlutusInterface.Estimate (
  estimateBudget,
) where

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo))
import BotPlutusInterface.QueryNode qualified as QueryNode
import BotPlutusInterface.Types (
  BudgetEstimationError (..),
  MintBudgets,
  PABConfig (pcNetwork),
  SpendBudgets,
  TxBudget (TxBudget),
  TxFile (..),
 )
import Cardano.Api qualified as CAPI
import Control.Arrow (left)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), MintingPolicyHash, TxOutRef)
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, fromCardanoTxIn)
import System.Directory.Internal.Prelude (getEnv)
import Prelude

{- | Estimate budget of transaction.
 Returns separate budgets for spending and minting.
 Spending budgets mapped to `TxOutRef`s,
 minting to `MintingPolicyHash`'es
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

  let txBudget = do
        body <- txBody
        budget <- budgetRes
        (spendingBudgets, policyBudgets) <- mkBudgetMaps budget body
        Right $ TxBudget spendingBudgets policyBudgets

  return txBudget

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

-- | Shorthand alias
type ExUnitsMap =
  Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError CAPI.ExecutionUnits)

-- | Calculate execution units using `Cardano.Api``
getExUnits ::
  NodeInfo ->
  CAPI.TxBody CAPI.AlonzoEra ->
  IO (Either BudgetEstimationError ExUnitsMap)
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

{- | Converts `ExecutionUnits` returned by `Cardano.Api` to `ExBudget`
  and maps each budget to corresponding spending input or minting policy
-}
mkBudgetMaps ::
  ExUnitsMap ->
  CAPI.TxBody CAPI.AlonzoEra ->
  Either BudgetEstimationError (SpendBudgets, MintBudgets)
mkBudgetMaps exUnitsMap txBody = do
  let (CAPI.TxBody txbc) = txBody
      insIx = mkInputsIndex txbc
      policiesIx = mkPoliciesIndex txbc

  exUnits <-
    left ScriptFailure $
      mapM
        (\(swix, a) -> (swix,) <$> a)
        (Map.toList exUnitsMap)

  -- perform lookups in `insIx` and `policiesIx` to map
  -- `TxOutRef`'s and `MintingPolicyHash`'es to corresponding `ExBudget`s
  mconcat <$> mapM (f insIx policiesIx) exUnits
  where
    mkInputsIndex =
      Map.fromList
        . zip [0 ..]
        -- This relies on the TxId Ord instance being consistent with the
        -- Ledger.TxId Ord instance via the toShelleyTxId conversion
        -- This is checked by prop_ord_distributive_TxId
        . sort
        . map fst
        . CAPI.txIns

    mkPoliciesIndex txbc =
      case CAPI.txMintValue txbc of
        CAPI.TxMintValue _ value _ ->
          -- The minting policies are indexed in policy id order in the value -- TODO: link to source
          let CAPI.ValueNestedRep bundle = CAPI.valueToNestedRep value
           in Map.fromList
                [ (ix, policyId)
                | (ix, CAPI.ValueNestedBundle policyId _) <- zip [0 ..] bundle
                ]
        _ -> mempty

    {- lookup for single calculated budget:
       if not found in either of indexes, return error
       if found - convert to `ExBudget`
                  and map to corresponding `TxOutRef` or `MintingPolicyHash`
    -}
    f ::
      Map Integer CAPI.TxIn ->
      Map Integer CAPI.PolicyId ->
      (CAPI.ScriptWitnessIndex, CAPI.ExecutionUnits) ->
      Either BudgetEstimationError (Map TxOutRef ExBudget, Map MintingPolicyHash ExBudget)
    f insIx policiesIx budgetItem
      | (CAPI.ScriptWitnessIndexTxIn ix, eu) <- budgetItem =
        case Map.lookup (toInteger ix) insIx of
          Nothing -> Left $ BudgetNotFound (CAPI.ScriptWitnessIndexTxIn ix)
          Just inp ->
            Right . (,mempty) $
              Map.singleton (fromCardanoTxIn inp) (unitsToBudget eu)
      | (CAPI.ScriptWitnessIndexMint ix, eu) <- budgetItem =
        case Map.lookup (toInteger ix) policiesIx of
          Nothing -> Left $ BudgetNotFound (CAPI.ScriptWitnessIndexTxIn ix)
          Just pId ->
            Right . (mempty,) $
              Map.singleton (fromCardanoPolicyId pId) (unitsToBudget eu)
      | otherwise = Right mempty

-- | Cardano to Plutus budget converter
unitsToBudget :: CAPI.ExecutionUnits -> ExBudget
unitsToBudget (CAPI.ExecutionUnits cpu mem) =
  ExBudget (ExCPU $ cast cpu) (ExMemory $ cast mem)
  where
    cast = fromInteger . toInteger

-- | Helper error converter
toBudgetError :: Show e => e -> BudgetEstimationError
toBudgetError = BudgetEstimationError . Text.pack . show
