{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.Balance (
  BalanceTxConstraint (TxWithScript, TxWithoutScript, TxWithSeparateChange),
  balanceTxStep,
  balanceTxIO,
  balanceTxIO',
  txUsesScripts,
  withFee,
) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (
  PABEffect,
  createDirectoryIfMissingCLI,
  getInMemCollateral,
  posixTimeRangeToContainedSlotRange,
  printBpiLog,
 )
import BotPlutusInterface.Files (DummyPrivKey, unDummyPrivateKey)
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (CollateralUtxo, LogLevel (Debug), PABConfig, collateralTxOutRef)
import Cardano.Api (ExecutionUnitPrices (ExecutionUnitPrices))
import Cardano.Api.Shelley (ProtocolParameters (protocolParamPrices))
import Control.Monad (foldM, void, zipWithM)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT, runEitherT)
import Control.Monad.Trans.Except (throwE)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Either.Combinators (rightToMaybe)
import Data.Kind (Type)
import Data.List ((\\))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (Proxy))
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

import BotPlutusInterface.Collateral (removeCollateralFromMap)
import Prettyprinter (pretty, viaShow, (<+>))
import Prelude

data TxWithScript
data TxWithoutScript

data BalanceTxConstraint a where
  TxWithScript :: BalanceTxConstraint TxWithScript
  TxWithoutScript :: BalanceTxConstraint TxWithoutScript
  TxWithSeparateChange :: BalanceTxConstraint a

instance Eq (BalanceTxConstraint a) where
  TxWithScript == TxWithScript = True
  TxWithoutScript == TxWithoutScript = True
  TxWithSeparateChange == TxWithSeparateChange = True
  _ == _ = False

class KnownBalanceConstraint (a :: Type) where
  knownConstraint :: proxy a -> BalanceTxConstraint a

instance KnownBalanceConstraint TxWithScript where
  knownConstraint _ = TxWithScript

instance KnownBalanceConstraint TxWithoutScript where
  knownConstraint _ = TxWithoutScript

{- | Collect necessary tx inputs and collaterals, add minimum lovelace values and balance non ada
 assets. `balanceTxIO` assumes that the `Tx` we are balancing is of type `TxWithoutScript`.
 If you want to add custom constraints while balancing the `Tx` like, saying that `Tx` contains
 scripts refer to `balanceTxIO'`.
-}
balanceTxIO ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  Eff effs (Either Text Tx)
balanceTxIO = balanceTxIO' @w [TxWithoutScript]

{- | This is just a more flexible version of `balanceTxIO` which let's specify the `BalanceTxConstraint`
 -   for the `Tx` that we are balancing.
-}
balanceTxIO' ::
  forall (w :: Type) (effs :: [Type -> Type]) (a :: Type).
  (Member (PABEffect w) effs, KnownBalanceConstraint a) =>
  [BalanceTxConstraint a] ->
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  Eff effs (Either Text Tx)
balanceTxIO' balanceTxconstraints pabConf ownPkh unbalancedTx =
  runEitherT $
    do
      (utxos, mcollateral) <- newEitherT $ utxosAndCollateralAtAddress @w balanceTxconstraints pabConf changeAddr
      privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf

      let utxoIndex = fmap Tx.toTxOut utxos <> unBalancedTxUtxoIndex unbalancedTx
          requiredSigs = map Ledger.unPaymentPubKeyHash $ Map.keys (unBalancedTxRequiredSignatories unbalancedTx)
          txType = knownConstraint @a Proxy

      lift $ printBpiLog @w Debug $ viaShow utxoIndex

      -- We need this folder on the CLI machine, which may not be the local machine
      lift $ createDirectoryIfMissingCLI @w False (Text.unpack "pcTxFileDir")

      tx <-
        newEitherT $
          addValidRange @w
            (unBalancedTxValidityTimeRange unbalancedTx)
            (unBalancedTxTx unbalancedTx)

      -- Adds required collaterals in the `Tx`, if the `Tx` is of type `TxWithScript`.
      -- Also adds signatures for fee calculation
      preBalancedTx <- case txType of
        TxWithScript ->
          maybe
            (throwE "Tx uses script but no collateral was provided.")
            (hoistEither . addSignatories ownPkh privKeys requiredSigs . flip addTxCollaterals tx)
            mcollateral
        _ -> hoistEither $ addSignatories ownPkh privKeys requiredSigs tx

      -- Balance the tx
      (balancedTx, minUtxos) <- balanceTxLoop utxoIndex privKeys [] preBalancedTx

      -- Get current Ada change
      let adaChange = getAdaChange utxoIndex balancedTx
          bTx = fst <$> balanceTxLoop utxoIndex privKeys minUtxos (addOutput changeAddr balancedTx)

      balancedTxWithChange <-
        case adaChange /= 0 of
          True | TxWithSeparateChange `elem` balanceTxconstraints -> bTx
          True | not (hasChangeUTxO changeAddr balancedTx) -> bTx
          True -> pure balancedTx
          False -> pure balancedTx

      -- Get the updated change, add it to the tx
      let finalAdaChange = getAdaChange utxoIndex balancedTxWithChange
          fullyBalancedTx = addAdaChange balanceTxconstraints changeAddr finalAdaChange balancedTxWithChange

      -- finally, we must update the signatories
      hoistEither $ addSignatories ownPkh privKeys requiredSigs fullyBalancedTx
  where
    changeAddr :: Address
    changeAddr = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) pabConf.pcOwnStakePubKeyHash

    balanceTxLoop ::
      Map TxOutRef TxOut ->
      Map PubKeyHash DummyPrivKey ->
      [(TxOut, Integer)] ->
      Tx ->
      EitherT Text (Eff effs) (Tx, [(TxOut, Integer)])
    balanceTxLoop utxoIndex privKeys prevMinUtxos tx = do
      void $ lift $ Files.writeAll @w pabConf tx
      nextMinUtxos <-
        newEitherT $
          calculateMinUtxos @w pabConf (Tx.txData tx) $ Tx.txOutputs tx \\ map fst prevMinUtxos

      let minUtxos = prevMinUtxos ++ nextMinUtxos

      lift $ printBpiLog @w Debug $ "Min utxos:" <+> pretty minUtxos

      -- Calculate fees by pre-balancing the tx, building it, and running the CLI on result
      txWithoutFees <-
        hoistEither $ balanceTxStep minUtxos utxoIndex changeAddr $ tx `withFee` 0

      exBudget <- newEitherT $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys txWithoutFees

      nonBudgettedFees <- newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

      let fees = nonBudgettedFees + getBudgetPrice (getExecutionUnitPrices pabConf) exBudget

      lift $ printBpiLog @w Debug $ "Fees:" <+> pretty fees

      -- Rebalance the initial tx with the above fees
      balancedTx <- hoistEither $ balanceTxStep minUtxos utxoIndex changeAddr $ tx `withFee` fees

      if balancedTx == tx
        then pure (balancedTx, minUtxos)
        else balanceTxLoop utxoIndex privKeys minUtxos balancedTx

-- `utxosAndCollateralAtAddress` returns all the utxos that can be used as input of a `Tx`,
-- i.e. we filter out `CollateralUtxo` present at the user's address, so it can't be used as input of a `Tx`.
-- This function throws error if the `Tx` type is of `BalanceTxWithScripts` but there's not `CollateralUtxo`
-- in the environment.
utxosAndCollateralAtAddress ::
  forall (w :: Type) (effs :: [Type -> Type]) (a :: Type).
  (Member (PABEffect w) effs, KnownBalanceConstraint a) =>
  [BalanceTxConstraint a] ->
  PABConfig ->
  Address ->
  Eff effs (Either Text (Map TxOutRef Tx.ChainIndexTxOut, Maybe CollateralUtxo))
utxosAndCollateralAtAddress _ pabConf changeAddr =
  runEitherT $ do
    utxos <- newEitherT $ CardanoCLI.utxosAt @w pabConf changeAddr
    inMemCollateral <- lift $ getInMemCollateral @w

    case knownConstraint @a Proxy of
      TxWithScript ->
        maybe
          ( throwE $
              "The given transaction uses script, but there's no collateral provided."
                <> "This usually means that, we failed to create Tx and update our ContractEnvironment."
          )
          (const $ pure (removeCollateralFromMap inMemCollateral utxos, inMemCollateral))
          inMemCollateral
      TxWithoutScript -> pure (removeCollateralFromMap inMemCollateral utxos, Nothing)
      _ -> pure (utxos, Nothing)

hasChangeUTxO :: Address -> Tx -> Bool
hasChangeUTxO changeAddr tx =
  any check $ txOutputs tx
  where
    check :: TxOut -> Bool
    check txOut =
      Tx.txOutAddress txOut == changeAddr

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

-- | Set collateral or fail in case it's required but not available
addTxCollaterals :: CollateralUtxo -> Tx -> Tx
addTxCollaterals cOut tx
  | txUsesScripts tx = tx {txCollateral = Set.singleton (Tx.pubKeyTxIn (collateralTxOutRef cOut))}
  | otherwise = tx

txUsesScripts :: Tx -> Bool
txUsesScripts Tx {txInputs, txMintScripts} =
  not (null txMintScripts)
    || any
      (\TxIn {txInType} -> case txInType of Just ConsumeScriptAddress {} -> True; _ -> False)
      (Set.toList txInputs)

-- | Ensures all non ada change goes back to user
handleNonAdaChange :: Address -> Map TxOutRef TxOut -> Tx -> Either Text Tx
handleNonAdaChange changeAddr utxos tx =
  let nonAdaChange = getNonAdaChange utxos tx
      newOutput =
        TxOut
          { txOutAddress = changeAddr
          , txOutValue = nonAdaChange
          , txOutDatumHash = Nothing
          }
      outputs =
        modifyFirst
          ((==) changeAddr . Tx.txOutAddress)
          (Just . maybe newOutput (addValueToTxOut nonAdaChange))
          (txOutputs tx)
   in if isValueNat nonAdaChange
        then Right $ if Value.isZero nonAdaChange then tx else tx {txOutputs = outputs}
        else Left "Not enough inputs to balance tokens."

{- | `addAdaChange` checks if `TxWithSeparateChange` is the present in the provided balancing
  constraints, if it is then we add the ada change to seperate `TxOut`, else we add it to
  any `TxOut` present at changeAddr.
-}
addAdaChange :: forall (a :: Type). [BalanceTxConstraint a] -> Address -> Integer -> Tx -> Tx
addAdaChange _ _ 0 tx = tx
addAdaChange balanceTxconstraints changeAddr change tx
  | TxWithSeparateChange `elem` balanceTxconstraints =
    tx
      { txOutputs =
          List.reverse $
            modifyFirst
              (\txout -> Tx.txOutAddress txout == changeAddr && justLovelace (txOutValue txout))
              (fmap $ addValueToTxOut $ Ada.lovelaceValueOf change)
              (List.reverse $ txOutputs tx)
      }
  | otherwise =
    tx
      { txOutputs =
          modifyFirst
            ((== changeAddr) . Tx.txOutAddress)
            (fmap $ addValueToTxOut $ Ada.lovelaceValueOf change)
            (txOutputs tx)
      }

addValueToTxOut :: Value -> TxOut -> TxOut
addValueToTxOut val txOut = txOut {txOutValue = txOutValue txOut <> val}

-- | Adds a 1 lovelace output to a transaction
addOutput :: Address -> Tx -> Tx
addOutput changeAddr tx = tx {txOutputs = txOutputs tx ++ [changeTxOut]}
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

addValidRange ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  POSIXTimeRange ->
  Tx ->
  Eff effs (Either Text Tx)
addValidRange timeRange tx =
  if validateRange timeRange
    then
      bimap (Text.pack . show) (setRange tx)
        <$> posixTimeRangeToContainedSlotRange @w timeRange
    else pure $ Left "Invalid validity interval."
  where
    setRange tx' range = tx' {txValidRange = range}

validateRange :: forall (a :: Type). Ord a => Interval a -> Bool
validateRange (Interval (LowerBound PosInf _) _) = False
validateRange (Interval _ (UpperBound NegInf _)) = False
validateRange (Interval (LowerBound (Finite lowerBound) _) (UpperBound (Finite upperBound) _))
  | lowerBound >= upperBound = False
validateRange _ = True

{- | Modifies the first element matching a predicate, or, if none found, call the modifier with Nothing
 Calling this function ensures the modifier will always be run once
-}
modifyFirst ::
  forall (a :: Type).
  -- | Predicate for value to update
  (a -> Bool) ->
  -- | Modifier, input Maybe representing existing value (or Nothing if missing), output value representing new value (or Nothing to remove)
  (Maybe a -> Maybe a) ->
  [a] ->
  [a]
modifyFirst _ m [] = m Nothing `consJust` []
modifyFirst p m (x : xs) = if p x then m (Just x) `consJust` xs else x : modifyFirst p m xs

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

justLovelace :: Value -> Bool
justLovelace value = length (Value.flattenValue value) == 1 && lovelaceValue value /= 0

consJust :: forall (a :: Type). Maybe a -> [a] -> [a]
consJust (Just x) = (x :)
consJust _ = id
