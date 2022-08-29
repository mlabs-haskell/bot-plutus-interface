-- | Module contains implementation for `estimateBudget` effect
module BotPlutusInterface.ExBudget (
  estimateBudget,
) where

import BotPlutusInterface.CardanoNode.Query (
  QueryConstraint,
  connectionInfo,
  queryBabbageEra,
  queryInCardanoMode,
 )
import BotPlutusInterface.Types (
  BudgetEstimationError (..),
  MintBudgets,
  PABConfig,
  SpendBudgets,
  TxBudget (TxBudget),
  TxFile (..),
 )
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Cardano.Prelude (maybeToEither)
import Control.Arrow (left)
import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Reader (runReader)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), MintingPolicyHash, TxOutRef)
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, fromCardanoTxIn)
import Relude hiding (runReader)

{- | Estimate budget of transaction.
 Returns separate budgets for spending and minting.
 Spending budgets mapped to `TxOutRef`s,
 minting to `MintingPolicyHash`'es
-}
estimateBudget :: PABConfig -> TxFile -> IO (Either BudgetEstimationError TxBudget)
estimateBudget pabConf txFile = do
  txBody <- case txFile of
    Raw rp -> deserialiseRaw rp
    Signed sp -> fmap CApi.getTxBody <$> deserialiseSigned sp

  budgetRes <-
    either
      (pure . Left)
      (getExUnits pabConf)
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
getScaledBudget :: CApi.ExecutionUnits -> Rational -> ExUnitsMap -> Either BudgetEstimationError ExUnitsMap
getScaledBudget maxUnits scaler budget =
  if fst scalers >= 1 && snd scalers >= 1
    then Right $ fmap (fmap $ scaleBudget scalers) budget
    else
      Left $
        BudgetEstimationError $
            "Exceeded global transaction budget\nCalculated: " <> show budgetSum <> "\nLimit: " <> show maxUnits
  where
    budgetSum = foldr addBudgets (CApi.ExecutionUnits 0 0) $ rights $ Map.elems budget
    scalers =
      ( clampedScaler (CApi.executionSteps budgetSum) (CApi.executionSteps maxUnits) scaler
      , clampedScaler (CApi.executionMemory budgetSum) (CApi.executionMemory maxUnits) scaler
      )

clampedScaler :: Natural -> Natural -> Rational -> Rational
clampedScaler 0 _ scaler = scaler
clampedScaler val maxVal scaler = min scaler (toRational maxVal / toRational val)

-- | Scale the budget by the multipliers in config
scaleBudget :: (Rational, Rational) -> CApi.ExecutionUnits -> CApi.ExecutionUnits
scaleBudget (stepsScaler, memScaler) (CApi.ExecutionUnits steps mem) = CApi.ExecutionUnits (scale steps stepsScaler) (scale mem memScaler)
  where
    scale x scaler = round $ toRational x * scaler

addBudgets :: CApi.ExecutionUnits -> CApi.ExecutionUnits -> CApi.ExecutionUnits
addBudgets (CApi.ExecutionUnits steps mem) (CApi.ExecutionUnits steps' mem') = CApi.ExecutionUnits (steps + steps') (mem + mem')

-- | Deserialize transaction body from ".signed" file
deserialiseSigned :: FilePath -> IO (Either BudgetEstimationError (CApi.Tx CApi.BabbageEra))
deserialiseSigned txFile = do
  envlp <- readEnvelope
  pure $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CApi.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CApi.deserialiseFromTextEnvelope (CApi.AsTx CApi.AsBabbageEra)

-- | Deserialize transaction body from ".raw" file
deserialiseRaw :: FilePath -> IO (Either BudgetEstimationError (CApi.TxBody CApi.BabbageEra))
deserialiseRaw txFile = do
  envlp <- readEnvelope
  pure $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CApi.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CApi.deserialiseFromTextEnvelope (CApi.AsTxBody CApi.AsBabbageEra)

-- | Shorthand alias
type ExUnitsMap =
  Map CApi.ScriptWitnessIndex (Either CApi.ScriptExecutionError CApi.ExecutionUnits)

-- | Calculate execution units using `Cardano.Api``
getExUnits ::
  PABConfig ->
  CApi.TxBody CApi.BabbageEra ->
  IO (Either BudgetEstimationError ExUnitsMap)
getExUnits pabConf txBody = do
  conn <- connectionInfo pabConf
  runM $ runReader conn (getExUnits' txBody)

getExUnits' ::
  QueryConstraint effs =>
  CApi.TxBody CApi.BabbageEra ->
  Eff effs (Either BudgetEstimationError ExUnitsMap)
getExUnits' txBody = do
  sysStart <- queryInCardanoMode CApi.QuerySystemStart
  eraHistory <- queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  pparams <- queryBabbageEra CApi.QueryProtocolParameters
  utxo <- queryBabbageEra $ CApi.QueryUTxO (CApi.QueryUTxOByTxIn $ Set.fromList capiIns)
  pure $
    flattenEvalResult $
      CApi.evaluateTransactionExecutionUnits CApi.BabbageEraInCardanoMode
        <$> sysStart
        <*> eraHistory
        <*> pparams
        <*> utxo
        <*> pure txBody
  where
    capiIns :: [CApi.TxIn]
    capiIns =
      let (CApi.TxBody txbc) = txBody
       in fst <$> CApi.txIns txbc

    flattenEvalResult = \case
      Right (Right res) -> Right res
      err -> Left $ toBudgetError err

{- | Converts `ExecutionUnits` returned by `Cardano.Api` to `ExBudget`
  and maps each budget to corresponding spending input or minting policy
-}
mkBudgetMaps ::
  ExUnitsMap ->
  CApi.TxBody CApi.BabbageEra ->
  Either BudgetEstimationError (SpendBudgets, MintBudgets)
mkBudgetMaps exUnitsMap txBody = do
  let (CApi.TxBody txbc) = txBody
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
        . CApi.txIns

    mkPoliciesIndex txbc =
      case CApi.txMintValue txbc of
        CApi.TxMintValue _ value _ ->
          {- The minting policies are indexed in policy id order in the value
             reference:
             https://github.com/input-output-hk/cardano-node/blob/e31455eaeca98530ce561b79687a8e465ebb3fdd/cardano-api/src/Cardano/Api/TxBody.hs#L2881
          -}
          let CApi.ValueNestedRep bundle = CApi.valueToNestedRep value
           in Map.fromList
                [ (ix, policyId)
                | (ix, CApi.ValueNestedBundle policyId _) <- zip [0 ..] bundle
                ]
        _ -> mempty

    {- lookup for single calculated budget:
       if not found in either of indexes, return error
       if found - convert to `ExBudget`
                  and map to corresponding `TxOutRef` or `MintingPolicyHash`
    -}
    f ::
      Map Integer CApi.TxIn ->
      Map Integer CApi.PolicyId ->
      (CApi.ScriptWitnessIndex, CApi.ExecutionUnits) ->
      Either BudgetEstimationError (Map TxOutRef ExBudget, Map MintingPolicyHash ExBudget)
    f insIx policiesIx budgetItem
      | (CApi.ScriptWitnessIndexTxIn ix, eu) <- budgetItem =
        case Map.lookup (toInteger ix) insIx of
          Nothing -> Left $ BudgetNotFound (CApi.ScriptWitnessIndexTxIn ix)
          Just inp ->
            Right . (,mempty) $
              Map.singleton (fromCardanoTxIn inp) (unitsToBudget eu)
      | (CApi.ScriptWitnessIndexMint ix, eu) <- budgetItem =
        case Map.lookup (toInteger ix) policiesIx of
          Nothing -> Left $ BudgetNotFound (CApi.ScriptWitnessIndexTxIn ix)
          Just pId ->
            Right . (mempty,) $
              Map.singleton (fromCardanoPolicyId pId) (unitsToBudget eu)
      | otherwise = Right mempty

-- | Cardano to Plutus budget converter
unitsToBudget :: CApi.ExecutionUnits -> ExBudget
unitsToBudget (CApi.ExecutionUnits cpu mem) =
  ExBudget (ExCPU $ cast cpu) (ExMemory $ cast mem)
  where
    cast = fromInteger . toInteger

-- | Helper error converter
toBudgetError :: Show e => e -> BudgetEstimationError
toBudgetError = BudgetEstimationError . show
