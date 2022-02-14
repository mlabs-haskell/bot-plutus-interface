{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.Balance (
  balanceTxStep,
  balanceTxIO,
) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, createDirectoryIfMissing, printLog)
import BotPlutusInterface.Files (DummyPrivKey, unDummyPrivateKey)
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (LogLevel (Debug), PABConfig)
import Control.Monad (foldM, void, zipWithM)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT, runEitherT)
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
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Constraints.OffChain (UnbalancedTx (..), fromScriptOutput)
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
      utxos <- newEitherT $ CardanoCLI.utxosAt @w pabConf $ Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) Nothing
      privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf
      let utxoIndex = fmap Tx.toTxOut utxos <> fmap (Ledger.toTxOut . fromScriptOutput) (unBalancedTxUtxoIndex unbalancedTx)
          requiredSigs = map Ledger.unPaymentPubKeyHash $ Map.keys (unBalancedTxRequiredSignatories unbalancedTx)
      tx <-
        hoistEither $
          addValidRange
            pabConf
            (unBalancedTxValidityTimeRange unbalancedTx)
            (unBalancedTxTx unbalancedTx)

      lift $ printLog @w Debug $ show utxoIndex

      -- Adds required collaterals, only needs to happen once
      -- Also adds signatures for fee calculation
      preBalancedTx <- hoistEither $ addTxCollaterals utxoIndex tx >>= addSignatories ownPkh privKeys requiredSigs

      -- Balance the tx
      balancedTx <- loop utxoIndex privKeys [] preBalancedTx

      -- finally, we must update the signatories
      hoistEither $ addSignatories ownPkh privKeys requiredSigs balancedTx
  where
    loop ::
      Map TxOutRef TxOut ->
      Map PubKeyHash DummyPrivKey ->
      [(TxOut, Integer)] ->
      Tx ->
      EitherT Text (Eff effs) Tx
    loop utxoIndex privKeys prevMinUtxos tx = do
      void $ lift $ Files.writeAll @w pabConf tx
      nextMinUtxos <-
        newEitherT $
          calculateMinUtxos @w pabConf (Tx.txData tx) $ Tx.txOutputs tx \\ map fst prevMinUtxos

      let minUtxos = prevMinUtxos ++ nextMinUtxos

      lift $ printLog @w Debug $ "Min utxos: " ++ show minUtxos

      -- Calculate fees by pre-balancing the tx, building it, and running the CLI on result
      txWithoutFees <-
        hoistEither $ balanceTxStep minUtxos 0 utxoIndex ownPkh tx

      lift $ createDirectoryIfMissing @w False (Text.unpack pabConf.pcTxFileDir)
      newEitherT $ CardanoCLI.buildTx @w pabConf privKeys txWithoutFees
      fees <- newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

      lift $ printLog @w Debug $ "Fees: " ++ show fees

      -- Rebalance the initial tx with the above fees
      balancedTx <- hoistEither $ balanceTxStep minUtxos fees utxoIndex ownPkh tx

      let balanceTxWithFees = balancedTx {txFee = Ada.lovelaceValueOf fees}

      if balanceTxWithFees == tx
        then pure balanceTxWithFees
        else loop utxoIndex privKeys minUtxos balanceTxWithFees

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
  Integer ->
  Map TxOutRef TxOut ->
  PubKeyHash ->
  Tx ->
  Either Text Tx
balanceTxStep minUtxos fees utxos ownPkh tx =
  Right (addLovelaces minUtxos tx)
    >>= balanceTxIns utxos fees
    >>= handleChange ownPkh utxos fees

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

balanceTxIns :: Map TxOutRef TxOut -> Integer -> Tx -> Either Text Tx
balanceTxIns utxos fees tx = do
  let txOuts = Tx.txOutputs tx
      nonMintedValue = mconcat (map Tx.txOutValue txOuts) `minus` txMint tx
      minSpending =
        mconcat
          [ Ada.lovelaceValueOf fees
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

-- | Ensures all change goes back to user
handleChange :: PubKeyHash -> Map TxOutRef TxOut -> Integer -> Tx -> Either Text Tx
handleChange ownPkh utxos fees tx =
  let changeAddr = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) Nothing
      txInRefs = map Tx.txInRef $ Set.toList $ txInputs tx
      inputValue = mconcat $ map Tx.txOutValue $ mapMaybe (`Map.lookup` utxos) txInRefs
      outputValue = mconcat $ map Tx.txOutValue $ txOutputs tx
      nonMintedOutputValue = outputValue `minus` txMint tx
      change = (inputValue `minus` nonMintedOutputValue) `minus` Ada.lovelaceValueOf fees
      outputs =
        case partition ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx of
          ([], txOuts) ->
            TxOut
              { txOutAddress = changeAddr
              , txOutValue = change
              , txOutDatumHash = Nothing
              } :
            txOuts
          (txOut@TxOut {txOutValue = v} : txOuts, txOuts') ->
            txOut {txOutValue = v <> change} : (txOuts <> txOuts')
   in if isValueNat change
        then Right $ if Value.isZero change then tx else tx {txOutputs = outputs}
        else Left "Not enough inputs to balance tokens."

{- | Add the required signatorioes to the transaction. Be aware the the signature itself is invalid,
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
    then Right $ tx {txValidRange = posixTimeRangeToContainedSlotRange pabConf.pcSlotConfig timeRange}
    else Left "Invalid validity interval."

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
