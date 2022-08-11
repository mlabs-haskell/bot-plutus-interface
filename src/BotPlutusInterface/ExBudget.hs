-- | Module contains implementation for `estimateBudget` effect
module BotPlutusInterface.ExBudget (
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
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Cardano.Prelude (maybeToEither)
import Control.Arrow (left)
import Data.Either (rights)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.Natural (Natural)
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
estimateBudget pabConf txFile = do
  sock <- getEnv "CARDANO_NODE_SOCKET_PATH"
  let debugNodeInf = NodeInfo (pcNetwork pabConf) sock
  txBody <- case txFile of
    Raw rp -> deserialiseRaw rp
    Signed sp -> fmap CAPI.getTxBody <$> deserialiseSigned sp

  budgetRes <-
    either
      (pure . Left)
      (getExUnits debugNodeInf)
      txBody

  pure $
    do
      body <- txBody
      budget <- budgetRes
      pparams <-
        maybeToEither
          (BudgetEstimationError "No protocol params found")
          pabConf.pcProtocolParams
      maxUnits <-
        maybeToEither (BudgetEstimationError "Missing max units in parameters") $
          protocolParamMaxTxExUnits pparams

      scaledBudget <- getScaledBudget maxUnits pabConf.pcBudgetMultiplier budget

      (spendingBudgets, policyBudgets) <- mkBudgetMaps scaledBudget body

      Right $ TxBudget spendingBudgets policyBudgets

-- | Scale the budget clamping the total to the parameter limits
getScaledBudget :: CAPI.ExecutionUnits -> Rational -> ExUnitsMap -> Either BudgetEstimationError ExUnitsMap
getScaledBudget maxUnits scaler budget =
  if fst scalers >= 1 && snd scalers >= 1
    then Right $ fmap (fmap $ scaleBudget scalers) budget
    else
      Left $
        BudgetEstimationError $
          Text.pack $
            "Exceeded global transaction budget\nCalculated: " ++ show budgetSum ++ "\nLimit: " ++ show maxUnits
  where
    budgetSum = foldr addBudgets (CAPI.ExecutionUnits 0 0) $ rights $ Map.elems budget
    scalers =
      ( clampedScaler (CAPI.executionSteps budgetSum) (CAPI.executionSteps maxUnits) scaler
      , clampedScaler (CAPI.executionMemory budgetSum) (CAPI.executionMemory maxUnits) scaler
      )

clampedScaler :: Natural -> Natural -> Rational -> Rational
clampedScaler 0 _ scaler = scaler
clampedScaler val maxVal scaler = min scaler (toRational maxVal / toRational val)

-- | Scale the budget by the multipliers in config
scaleBudget :: (Rational, Rational) -> CAPI.ExecutionUnits -> CAPI.ExecutionUnits
scaleBudget (stepsScaler, memScaler) (CAPI.ExecutionUnits steps mem) = CAPI.ExecutionUnits (scale steps stepsScaler) (scale mem memScaler)
  where
    scale x scaler = round $ toRational x * scaler

addBudgets :: CAPI.ExecutionUnits -> CAPI.ExecutionUnits -> CAPI.ExecutionUnits
addBudgets (CAPI.ExecutionUnits steps mem) (CAPI.ExecutionUnits steps' mem') = CAPI.ExecutionUnits (steps + steps') (mem + mem')

-- | Deserialize transaction body from ".signed" file
deserialiseSigned :: FilePath -> IO (Either BudgetEstimationError (CAPI.Tx CAPI.BabbageEra))
deserialiseSigned txFile = do
  envlp <- readEnvelope
  pure $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope (CAPI.AsTx CAPI.AsBabbageEra)

-- | Deserialize transaction body from ".raw" file
deserialiseRaw :: FilePath -> IO (Either BudgetEstimationError (CAPI.TxBody CAPI.BabbageEra))
deserialiseRaw txFile = do
  envlp <- readEnvelope
  pure $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope (CAPI.AsTxBody CAPI.AsBabbageEra)

-- | Shorthand alias
type ExUnitsMap =
  Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError CAPI.ExecutionUnits)

-- | Calculate execution units using `Cardano.Api``
getExUnits ::
  NodeInfo ->
  CAPI.TxBody CAPI.BabbageEra ->
  IO (Either BudgetEstimationError ExUnitsMap)
getExUnits nodeInf txBody = do
  sysStart <- QueryNode.querySystemStart nodeInf
  eraHist <- QueryNode.queryEraHistory nodeInf
  pparams <- QueryNode.queryProtocolParams nodeInf
  utxo <- QueryNode.queryOutsByInputs nodeInf capiIns
  pure $
    flattenEvalResult $
      CAPI.evaluateTransactionExecutionUnits CAPI.BabbageEraInCardanoMode
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
  CAPI.TxBody CAPI.BabbageEra ->
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
        {- This relies on the TxId Ord instance being consistent with the
           Ledger.TxId Ord instance via the toShelleyTxId conversion
           This is checked by prop_ord_distributive_TxId
           reference:
           https://github.com/input-output-hk/cardano-node/blob/e31455eaeca98530ce561b79687a8e465ebb3fdd/cardano-api/src/Cardano/Api/TxBody.hs#L2887
        -}
        . sort
        . map fst -- get only `TxIn`'s from `TxIns` (which is list of tuples)
        . CAPI.txIns

    mkPoliciesIndex txbc =
      case CAPI.txMintValue txbc of
        CAPI.TxMintValue _ value _ ->
          {- The minting policies are indexed in policy id order in the value
             reference:
             https://github.com/input-output-hk/cardano-node/blob/e31455eaeca98530ce561b79687a8e465ebb3fdd/cardano-api/src/Cardano/Api/TxBody.hs#L2881
          -}
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
