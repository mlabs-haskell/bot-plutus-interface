{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.Balance (
  balanceTxStep,
  balanceTxIO,
  withFee,
) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, convertTimeRangeToSlotRange, createDirectoryIfMissingCLI, printLog)
import BotPlutusInterface.Files (DummyPrivKey, unDummyPrivateKey)
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (LogLevel (Debug), PABConfig)
import Cardano.Api (ExecutionUnitPrices (ExecutionUnitPrices))
import Cardano.Api.Shelley (ProtocolParameters (protocolParamPrices))
import Control.Monad (foldM, void, zipWithM)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT, runEitherT)
import Data.Coerce (coerce)
import Data.Either.Combinators (rightToMaybe)
import Data.Kind (Type)
import Data.List (partition, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (Ratio ((:%)))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Crypto (PubKeyHash)
import Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Ledger.Scripts (Datum, DatumHash)
import Ledger.Time (POSIXTimeRange)
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)
import Ledger.Tx (
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Tx qualified as Tx
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (..),
  TokenName (..),
 )

import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import Debug.Trace (traceM, traceShowId)
import Prelude

{- | Collect necessary tx inputs and collaterals, add minimum lovelace values and balance non ada
 assets
-}
balanceTxIO ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  Eff effs (Either Text Tx)
balanceTxIO pabConf ownPkh unbalancedTx =
  runEitherT $
    do
      utxos <- newEitherT $ CardanoCLI.utxosAt @w pabConf changeAddr
      privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf
      let utxoIndex = fmap Tx.toTxOut utxos <> unBalancedTxUtxoIndex unbalancedTx
          requiredSigs = map Ledger.unPaymentPubKeyHash $ Map.keys (unBalancedTxRequiredSignatories unbalancedTx)
      -- tx <-
      --   hoistEither $
      --     addValidRange
      --       pabConf
      --       (unBalancedTxValidityTimeRange unbalancedTx)
      --       (unBalancedTxTx unbalancedTx)

      tx <-
        newEitherT $
          addValidRange2 @w
            pabConf
            (unBalancedTxValidityTimeRange unbalancedTx)
            (unBalancedTxTx unbalancedTx)

      lift $ printLog @w Debug $ show utxoIndex

      -- We need this folder on the CLI machine, which may not be the local machine
      lift $ createDirectoryIfMissingCLI @w False (Text.unpack pabConf.pcTxFileDir)

      -- Adds required collaterals, only needs to happen once
      -- Also adds signatures for fee calculation
      preBalancedTx <- hoistEither $ addTxCollaterals utxoIndex tx >>= addSignatories ownPkh privKeys requiredSigs

      -- Balance the tx
      (balancedTx, minUtxos) <- loop utxoIndex privKeys [] preBalancedTx

      -- Get current Ada change
      let adaChange = getAdaChange utxoIndex balancedTx
      -- If we have change but no change UTxO, we need to add an output for it
      -- We'll add a minimal output, run the loop again so it gets minUTxO, then update change
      balancedTxWithChange <-
        if adaChange /= 0 && not (hasChangeUTxO changeAddr balancedTx)
          then fst <$> loop utxoIndex privKeys minUtxos (addOutput changeAddr balancedTx)
          else pure balancedTx

      -- Get the updated change, add it to the tx
      let finalAdaChange = getAdaChange utxoIndex balancedTxWithChange
          fullyBalancedTx = addAdaChange changeAddr finalAdaChange balancedTxWithChange

      -- finally, we must update the signatories
      hoistEither $ addSignatories ownPkh privKeys requiredSigs fullyBalancedTx
  where
    changeAddr :: Address
    changeAddr = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) (pabConf.pcOwnStakePubKeyHash)
    loop ::
      Map TxOutRef TxOut ->
      Map PubKeyHash DummyPrivKey ->
      [(TxOut, Integer)] ->
      Tx ->
      EitherT Text (Eff effs) (Tx, [(TxOut, Integer)])
    loop utxoIndex privKeys prevMinUtxos tx = do
      void $ lift $ Files.writeAll @w pabConf tx
      nextMinUtxos <-
        newEitherT $
          calculateMinUtxos @w pabConf (Tx.txData tx) $ Tx.txOutputs tx \\ map fst prevMinUtxos

      let minUtxos = prevMinUtxos ++ nextMinUtxos

      lift $ printLog @w Debug $ "Min utxos: " ++ show minUtxos

      -- Calculate fees by pre-balancing the tx, building it, and running the CLI on result
      txWithoutFees <-
        hoistEither $ balanceTxStep minUtxos utxoIndex changeAddr $ tx `withFee` 0

      exBudget <- newEitherT $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys txWithoutFees

      nonBudgettedFees <- newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

      let fees = nonBudgettedFees + getBudgetPrice (getExecutionUnitPrices pabConf) exBudget

      lift $ printLog @w Debug $ "Fees: " ++ show fees

      -- Rebalance the initial tx with the above fees
      balancedTx <- hoistEither $ balanceTxStep minUtxos utxoIndex changeAddr $ tx `withFee` fees

      if balancedTx == tx
        then pure (balancedTx, minUtxos)
        else loop utxoIndex privKeys minUtxos balancedTx

getExecutionUnitPrices :: PABConfig -> ExecutionUnitPrices
getExecutionUnitPrices pabConf = fromMaybe (ExecutionUnitPrices 0 0) $ protocolParamPrices pabConf.pcProtocolParams

getBudgetPrice :: ExecutionUnitPrices -> Ledger.ExBudget -> Integer
getBudgetPrice (ExecutionUnitPrices cpuPrice memPrice) (Ledger.ExBudget cpuUsed memUsed) =
  round cpuCost + round memCost
  where
    cpuCost = cpuPrice `multRational` (toInteger @Ledger.SatInt $ coerce cpuUsed)
    memCost = memPrice `multRational` (toInteger @Ledger.SatInt $ coerce memUsed)

multRational :: Rational -> Integer -> Rational
multRational (num :% denom) s = (s * num) :% denom

withFee :: Tx -> Integer -> Tx
withFee tx fee = tx {txFee = Ada.lovelaceValueOf fee}

calculateMinUtxos ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map DatumHash Datum ->
  [TxOut] ->
  Eff effs (Either Text [(TxOut, Integer)])
calculateMinUtxos pabConf datums txOuts =
  zipWithM (fmap . (,)) txOuts <$> mapM (CardanoCLI.calculateMinUtxo @w pabConf datums) txOuts

balanceTxStep ::
  [(TxOut, Integer)] ->
  Map TxOutRef TxOut ->
  Address ->
  Tx ->
  Either Text Tx
balanceTxStep minUtxos utxos changeAddr tx =
  Right (addLovelaces minUtxos tx)
    >>= balanceTxIns utxos
    >>= handleNonAdaChange changeAddr utxos

-- | Get change value of a transaction, taking inputs, outputs, mint and fees into account
getChange :: Map TxOutRef TxOut -> Tx -> Value
getChange utxos tx =
  let fees = lovelaceValue $ txFee tx
      txInRefs = map Tx.txInRef $ Set.toList $ txInputs tx
      inputValue = mconcat $ map Tx.txOutValue $ mapMaybe (`Map.lookup` utxos) txInRefs
      outputValue = mconcat $ map Tx.txOutValue $ txOutputs tx
      nonMintedOutputValue = outputValue `minus` txMint tx
      change = (inputValue `minus` nonMintedOutputValue) `minus` Ada.lovelaceValueOf fees
   in change

lovelaceValue :: Value -> Integer
lovelaceValue = flip Value.assetClassValueOf $ Value.assetClass "" ""

getAdaChange :: Map TxOutRef TxOut -> Tx -> Integer
getAdaChange utxos = lovelaceValue . getChange utxos

getNonAdaChange :: Map TxOutRef TxOut -> Tx -> Value
getNonAdaChange utxos = Ledger.noAdaValue . getChange utxos

-- | Getting the necessary utxos to cover the fees for the transaction
collectTxIns :: Set TxIn -> Map TxOutRef TxOut -> Value -> Either Text (Set TxIn)
collectTxIns originalTxIns utxos value =
  if isSufficient updatedInputs
    then Right updatedInputs
    else
      Left $
        Text.unlines
          [ "Insufficient tx inputs, needed: "
          , showText (Value.flattenValue value)
          , "got:"
          , showText (Value.flattenValue (txInsValue updatedInputs))
          ]
  where
    updatedInputs =
      foldl
        ( \acc txIn ->
            if isSufficient acc
              then acc
              else Set.insert txIn acc
        )
        originalTxIns
        $ mapMaybe (rightToMaybe . txOutToTxIn) $ Map.toList utxos

    isSufficient :: Set TxIn -> Bool
    isSufficient txIns' =
      not (Set.null txIns') && txInsValue txIns' `Value.geq` value

    txInsValue :: Set TxIn -> Value
    txInsValue txIns' =
      mconcat $ map Tx.txOutValue $ mapMaybe ((`Map.lookup` utxos) . Tx.txInRef) $ Set.toList txIns'

-- Converting a chain index transaction output to a transaction input type
txOutToTxIn :: (TxOutRef, TxOut) -> Either Text TxIn
txOutToTxIn (txOutRef, txOut) =
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ -> Right $ Tx.pubKeyTxIn txOutRef
    ScriptCredential _ -> Left "Cannot covert a script output to TxIn"

-- | Add min lovelaces to each tx output
addLovelaces :: [(TxOut, Integer)] -> Tx -> Tx
addLovelaces minLovelaces tx =
  let lovelacesAdded =
        map
          ( \txOut ->
              let outValue = txOutValue txOut
                  lovelaces = Ada.getLovelace $ Ada.fromValue outValue
                  minUtxo = fromMaybe 0 $ lookup txOut minLovelaces
               in txOut
                    { txOutValue =
                        outValue <> Ada.lovelaceValueOf (max 0 (minUtxo - lovelaces))
                    }
          )
          $ txOutputs tx
   in tx {txOutputs = lovelacesAdded}

balanceTxIns :: Map TxOutRef TxOut -> Tx -> Either Text Tx
balanceTxIns utxos tx = do
  let txOuts = Tx.txOutputs tx
      nonMintedValue = mconcat (map Tx.txOutValue txOuts) `minus` txMint tx
      minSpending =
        mconcat
          [ txFee tx
          , nonMintedValue
          ]
  txIns <- collectTxIns (txInputs tx) utxos minSpending
  pure $ tx {txInputs = txIns <> txInputs tx}

{- | Pick a collateral from the utxo map and add it to the unbalanced transaction
 (suboptimally we just pick a random utxo from the tx inputs)
-}
addTxCollaterals :: Map TxOutRef TxOut -> Tx -> Either Text Tx
addTxCollaterals utxos tx = do
  let txIns = mapMaybe (rightToMaybe . txOutToTxIn) $ Map.toList $ filterAdaOnly utxos
  txIn <- findPubKeyTxIn txIns
  pure $ tx {txCollateral = Set.singleton txIn}
  where
    findPubKeyTxIn = \case
      x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
      x@(TxIn _ Nothing) : _ -> Right x
      _ : xs -> findPubKeyTxIn xs
      _ -> Left "There are no utxos to be used as collateral"
    filterAdaOnly = Map.filter (isAdaOnly . txOutValue)

-- | Ensures all non ada change goes back to user
handleNonAdaChange :: Address -> Map TxOutRef TxOut -> Tx -> Either Text Tx
handleNonAdaChange changeAddr utxos tx =
  let nonAdaChange = getNonAdaChange utxos tx
      outputs =
        case partition ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx of
          ([], txOuts) ->
            TxOut
              { txOutAddress = changeAddr
              , txOutValue = nonAdaChange
              , txOutDatumHash = Nothing
              } :
            txOuts
          (txOut@TxOut {txOutValue = v} : txOuts, txOuts') ->
            txOut {txOutValue = v <> nonAdaChange} : (txOuts <> txOuts')
   in if isValueNat nonAdaChange
        then Right $ if Value.isZero nonAdaChange then tx else tx {txOutputs = outputs}
        else Left "Not enough inputs to balance tokens."

hasChangeUTxO :: Address -> Tx -> Bool
hasChangeUTxO changeAddr tx =
  any ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx

-- | Adds ada change to a transaction, assuming there is already an output going to ownPkh. Otherwise, this is identity
addAdaChange :: Address -> Integer -> Tx -> Tx
addAdaChange changeAddr change tx =
  tx
    { txOutputs =
        case partition ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx of
          (txOut@TxOut {txOutValue = v} : txOuts, txOuts') ->
            txOut {txOutValue = v <> Ada.lovelaceValueOf change} : (txOuts <> txOuts')
          _ -> txOutputs tx
    }

-- | Adds a 1 lovelace output to a transaction
addOutput :: Address -> Tx -> Tx
addOutput changeAddr tx = tx {txOutputs = changeTxOut : txOutputs tx}
  where
    changeTxOut =
      TxOut
        { txOutAddress = changeAddr
        , txOutValue = Ada.lovelaceValueOf 1
        , txOutDatumHash = Nothing
        }

{- | Add the required signatories to the transaction. Be aware the the signature itself is invalid,
 and will be ignored. Only the pub key hashes are used, mapped to signing key files on disk.
-}
addSignatories :: PubKeyHash -> Map PubKeyHash DummyPrivKey -> [PubKeyHash] -> Tx -> Either Text Tx
addSignatories ownPkh privKeys pkhs tx =
  foldM
    ( \tx' pkh ->
        case Map.lookup pkh privKeys of
          Just privKey -> Right $ Tx.addSignature' (unDummyPrivateKey privKey) tx'
          Nothing -> Left "Signing key not found."
    )
    tx
    (ownPkh : pkhs)

addValidRange :: PABConfig -> POSIXTimeRange -> Tx -> Either Text Tx
addValidRange pabConf timeRange tx =
  if validateRange timeRange
    then
      let r = traceShowId (posixTimeRangeToContainedSlotRange pabConf.pcSlotConfig timeRange)
       in Right $ tx {txValidRange = r}
    else Left "Invalid validity interval."

addValidRange2 ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  POSIXTimeRange ->
  Tx ->
  Eff effs (Either Text Tx)
addValidRange2 pabConf timeRange tx =
  if validateRange timeRange
    then do
      let oldWaySlotRange = posixTimeRangeToContainedSlotRange pabConf.pcSlotConfig timeRange
      traceM $ "Ledger SlotRange: " ++ show oldWaySlotRange
      newWaySlotRange <- convertTimeRangeToSlotRange @w timeRange
      case newWaySlotRange of
        Right range -> do
          traceM $ "Query  SlotRange: " ++ show range
          pure $ Right $ tx {txValidRange = range}
        Left err -> pure $ Left (Text.pack $ show err)
    else pure $ Left "Invalid validity interval."

validateRange :: forall (a :: Type). Ord a => Interval a -> Bool
validateRange (Interval (LowerBound PosInf _) _) = False
validateRange (Interval _ (UpperBound NegInf _)) = False
validateRange (Interval (LowerBound (Finite lowerBound) _) (UpperBound (Finite upperBound) _))
  | lowerBound >= upperBound = False
validateRange _ = True

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

minus :: Value -> Value -> Value
minus x y =
  let negativeValues = map (\(c, t, a) -> (c, t, - a)) $ Value.flattenValue y
   in x <> mconcat (map unflattenValue negativeValues)

unflattenValue :: (CurrencySymbol, TokenName, Integer) -> Value
unflattenValue (curSymbol, tokenName, amount) =
  Value.assetClassValue (Value.assetClass curSymbol tokenName) amount

isValueNat :: Value -> Bool
isValueNat =
  all (\(_, _, a) -> a >= 0) . Value.flattenValue

isAdaOnly :: Value -> Bool
isAdaOnly v =
  case Value.flattenValue v of
    [("", "", _)] -> True
    _ -> False
