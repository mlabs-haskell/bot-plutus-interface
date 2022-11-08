{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module BotPlutusInterface.Balance (
  BalanceConfig (BalanceConfig, bcSeparateChange),
  balanceTxStep,
  balanceTxIO,
  balanceTxIO',
  defaultBalanceConfig,
  txUsesScripts,
  withFee,
) where

import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.CardanoNode.Effects (NodeQuery (UtxosAt, UtxosAtExcluding))
import BotPlutusInterface.CoinSelection (selectTxIns)
import BotPlutusInterface.Effects (
  PABEffect,
  addValue,
  createDirectoryIfMissingCLI,
  getInMemCollateral,
  minUtxo,
  printBpiLog,
  queryNode,
 )
import BotPlutusInterface.Files (DummyPrivKey, unDummyPrivateKey)
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Helpers (addressTxOut, lovelaceValueOf)
import BotPlutusInterface.Types (
  CollateralUtxo (collateralTxOutRef),
  EstimationContext (..),
  LogLevel (Debug),
  LogType (TxBalancingLog),
  PABConfig,
  collateralTxOutRef,
  ownAddress,
  toExBudget,
 )
import Cardano.Api (ExecutionUnitPrices (ExecutionUnitPrices))
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (ProtocolParameters (protocolParamPrices))
import Control.Lens (folded, to, (&), (.~), (^.), (^..))
import Control.Monad (foldM, unless, void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State (State, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, bimapEitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Control.Monad.Trans.Except (throwE)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (Ratio ((:%)))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Crypto (PubKeyHash)
import Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Ledger.Tx (
  Tx (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI.Internal (toCardanoValue)
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  CurrencySymbol (..),
  TokenName (..),
 )

import Ledger.Constraints.OffChain qualified as Constraints
import Prettyprinter (pretty, viaShow, (<+>))
import Wallet.API qualified as WAPI
import Prelude

-- Config for balancing a `Tx`.
data BalanceConfig = BalanceConfig
  { -- | This field represents whether the ada change should be in separate UTxO.
    bcSeparateChange :: Bool
  }
  deriving stock (Show, Eq)

defaultBalanceConfig :: BalanceConfig
defaultBalanceConfig = BalanceConfig {bcSeparateChange = False}

{- | Collect necessary tx inputs and collaterals, add minimum lovelace values and balance non ada
     assets. `balanceTxIO` calls `balanceTxIO' with default `BalanceConfig`.
-}
balanceTxIO ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  EitherT WAPI.WalletAPIError (Eff effs) Tx
balanceTxIO = balanceTxIO' @w defaultBalanceConfig

-- | `balanceTxIO'` is more flexible version of `balanceTxIO`, this lets us specify custom `BalanceConfig`.
balanceTxIO' ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs) =>
  BalanceConfig ->
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  EitherT WAPI.WalletAPIError (Eff effs) Tx
balanceTxIO' balanceCfg pabConf pkh unbalancedTx =
  BodyBuilder.runInEstimationEffect @w (unBalancedEmulatorTx unbalancedTx) WAPI.OtherError $
    balanceTxIOEstimationContext @w balanceCfg pabConf pkh unbalancedTx

-- | `balanceTxIOEstimationContext` runs within the estimation context effect defined in BotPlutusInterface.BodyBuilder, see there for more information
balanceTxIOEstimationContext ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ( Member (PABEffect w) effs
  , Member (State EstimationContext) effs
  ) =>
  BalanceConfig ->
  PABConfig ->
  PubKeyHash ->
  UnbalancedTx ->
  EitherT WAPI.WalletAPIError (Eff effs) Tx
balanceTxIOEstimationContext balanceCfg pabConf ownPkh unbalancedTx' = do
  updatedOuts <-
    firstEitherT WAPI.OtherError $
      newEitherT $
        sequence <$> traverse (minUtxo @w) (unbalancedTx' ^. Constraints.tx . Tx.outputs)

  changeAddr <- hoistEither $ first WAPI.ToCardanoError $ ownAddress pabConf

  let unbalancedTx = unbalancedTx' & (Constraints.tx . Tx.outputs .~ updatedOuts)
      tx = unBalancedEmulatorTx unbalancedTx

  (utxoIndex, mcollateral) <-
    newEitherT $
      utxosAndCollateralAtAddress
        @w
        tx
        changeAddr

  privKeys <- firstEitherT WAPI.OtherError $ newEitherT $ Files.readPrivateKeys @w pabConf

  let requiredSigs :: [PubKeyHash]
      requiredSigs =
        unBalancedTxRequiredSignatories unbalancedTx
          ^.. folded . to Ledger.unPaymentPubKeyHash

  lift $ printBpiLog @w (Debug [TxBalancingLog]) $ viaShow utxoIndex

  -- We need this folder on the CLI machine, which may not be the local machine
  lift $ createDirectoryIfMissingCLI @w False (Text.unpack "pcTxFileDir")

  unless (validateRange $ txValidRange tx) $ throwE $ WAPI.OtherError "Invalid validity range on tx"

  -- Adds required collaterals in the `Tx`
  -- is true. Also adds signatures for fee calculation
  preBalancedTx <-
    hoistEither $
      addSignatories ownPkh privKeys requiredSigs $
        maybe tx (`addTxCollaterals` tx) mcollateral

  -- Balance the tx
  balancedTx <- balanceTxLoop utxoIndex privKeys changeAddr preBalancedTx
  changeTxOutWithMinAmt <- firstEitherT WAPI.OtherError $ newEitherT $ addOutput @w changeAddr balancedTx

  -- Get current Ada change
  let adaChange = getAdaChange utxoIndex balancedTx
      bTx = balanceTxLoop utxoIndex privKeys changeAddr changeTxOutWithMinAmt

  -- Checks if there's ada change left, if there is then we check
  -- if `bcSeparateChange` is true, if this is the case then we create a new UTxO at
  -- the changeAddr.
  balancedTxWithChange <-
    case adaChange /= 0 of
      True | bcSeparateChange balanceCfg || not (hasChangeUTxO changeAddr balancedTx) -> bTx
      _ -> pure balancedTx

  -- Get the updated change, add it to the tx
  let finalAdaChange = getAdaChange utxoIndex balancedTxWithChange
      fullyBalancedTx = addAdaChange balanceCfg changeAddr finalAdaChange balancedTxWithChange
      txInfoLog =
        printBpiLog @w (Debug [TxBalancingLog]) $
          "UnbalancedTx TxInputs: "
            <+> pretty (length $ txInputs preBalancedTx)
            <+> "UnbalancedTx TxOutputs: "
            <+> pretty (length $ txOutputs preBalancedTx)
            <+> "TxInputs: "
            <+> pretty (length $ txInputs fullyBalancedTx)
            <+> "TxOutputs: "
            <+> pretty (length $ txOutputs fullyBalancedTx)

  lift txInfoLog

  -- finally, we must update the signatories
  hoistEither $ addSignatories ownPkh privKeys requiredSigs fullyBalancedTx
  where
    balanceTxLoop ::
      Map TxOutRef TxOut ->
      Map PubKeyHash DummyPrivKey ->
      CApi.AddressInEra CApi.BabbageEra ->
      Tx ->
      EitherT WAPI.WalletAPIError (Eff effs) Tx
    balanceTxLoop utxoIndex privKeys changeAddr tx = do
      void $ lift $ Files.writeAll @w pabConf tx -- TODO: Does this really need to happen in the loop?

      -- Calculate fees by pre-balancing the tx, building it, and running the CLI on result
      txWithoutFees <-
        newEitherT $ balanceTxStep @w balanceCfg utxoIndex changeAddr $ tx `withFee` 0

      exBudget <- bimapEitherT WAPI.OtherError toExBudget $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys txWithoutFees

      nonBudgettedFees <- firstEitherT WAPI.OtherError $ newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

      let fees = nonBudgettedFees + getBudgetPrice (getExecutionUnitPrices pabConf) exBudget

      lift $ printBpiLog @w (Debug [TxBalancingLog]) $ "Fees:" <+> pretty fees

      -- Rebalance the initial tx with the above fees
      balancedTx <- newEitherT $ balanceTxStep @w balanceCfg utxoIndex changeAddr $ tx `withFee` fees

      if balancedTx == tx
        then pure balancedTx
        else balanceTxLoop utxoIndex privKeys changeAddr balancedTx

toCtxTxTxOut :: forall (era :: Type). CApi.TxOut CApi.CtxUTxO era -> CApi.TxOut CApi.CtxTx era
toCtxTxTxOut (CApi.TxOut addr val d refS) =
  let dat = case d of
        CApi.TxOutDatumNone -> CApi.TxOutDatumNone
        CApi.TxOutDatumHash s h -> CApi.TxOutDatumHash s h
        CApi.TxOutDatumInline s sd -> CApi.TxOutDatumInline s sd
   in CApi.TxOut addr val dat refS

-- `utxosAndCollateralAtAddress` returns all the utxos that can be used as an input of a `Tx`,
-- i.e. we filter out `CollateralUtxo` present at the user's address, so it can't be used as input of a `Tx`.
-- Also adds any new utxos to the estimation context
utxosAndCollateralAtAddress ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ( Member (PABEffect w) effs
  , Member (State EstimationContext) effs
  ) =>
  Tx ->
  CApi.AddressInEra CApi.BabbageEra ->
  Eff effs (Either WAPI.WalletAPIError (Map TxOutRef TxOut, Maybe CollateralUtxo))
utxosAndCollateralAtAddress tx changeAddr =
  runEitherT $ do
    inMemCollateral <- lift $ getInMemCollateral @w
    let nodeQuery =
          maybe
            (UtxosAt changeAddr)
            (UtxosAtExcluding changeAddr . Set.singleton . collateralTxOutRef)
            inMemCollateral

    -- TODO: This may actually need to include the collateral utxo, waiting on Misha's response
    utxos <- firstEitherT (WAPI.OtherError . Text.pack . show) $ newEitherT $ queryNode @w nodeQuery

    lift $ modify $ \(EstimationContext systemContext curUtxos) -> EstimationContext systemContext $ Map.union curUtxos utxos

    let utxos' = TxOut . toCtxTxTxOut <$> utxos

    -- check if transaction requires a collateral, if it does, search for
    -- collateral UTxO in the environment, error on missing input
    if txUsesScripts tx
      then
        maybe
          ( throwE $
              WAPI.OtherError $
                "The given transaction uses script, but there's no collateral provided."
                  <> "This usually means that, we failed to create Tx and update our ContractEnvironment."
          )
          (const $ pure (utxos', inMemCollateral))
          inMemCollateral
      else pure (utxos', Nothing)

hasChangeUTxO :: CApi.AddressInEra CApi.BabbageEra -> Tx -> Bool
hasChangeUTxO changeAddr tx =
  any check $ txOutputs tx
  where
    check :: TxOut -> Bool
    check txOut = txOutAddress txOut == changeAddr

getExecutionUnitPrices :: PABConfig -> ExecutionUnitPrices
getExecutionUnitPrices pabConf =
  fromMaybe (ExecutionUnitPrices 0 0) $
    pabConf.pcProtocolParams >>= protocolParamPrices

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

balanceTxStep ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  BalanceConfig ->
  Map TxOutRef TxOut ->
  CApi.AddressInEra CApi.BabbageEra ->
  Tx ->
  Eff effs (Either WAPI.WalletAPIError Tx)
balanceTxStep balanceCfg utxos changeAddr tx =
  runEitherT $
    (newEitherT . balanceTxIns @w utxos) tx
      >>= newEitherT . handleNonAdaChange @w balanceCfg changeAddr utxos

-- | Get change value of a transaction, taking inputs, outputs, mint and fees into account
getChange :: Map TxOutRef TxOut -> Tx -> CApi.Value
getChange utxos tx =
  let fees = ledgerLovelaceValue $ txFee tx
      txInputRefs = map Tx.txInputRef $ txInputs tx
      inputValue = mconcat $ map txOutValue $ mapMaybe (`Map.lookup` utxos) txInputRefs
      outputValue = mconcat $ map txOutValue $ txOutputs tx
      mintedValue = fromRight mempty $ toCardanoValue $ txMint tx
      nonMintedOutputValue = outputValue `minus` mintedValue
      change = (inputValue `minus` nonMintedOutputValue) `minus` lovelaceValueOf fees
   in change

ledgerLovelaceValue :: Value -> Integer
ledgerLovelaceValue v = Value.valueOf v "" ""

lovelaceValue :: CApi.Value -> Integer
lovelaceValue = (\(CApi.Quantity i) -> i) . flip CApi.selectAsset CApi.AdaAssetId

getAdaChange :: Map TxOutRef TxOut -> Tx -> Integer
getAdaChange utxos = lovelaceValue . getChange utxos

getNonAdaChange :: Map TxOutRef TxOut -> Tx -> CApi.Value
getNonAdaChange utxos = CApi.filterValue (/= CApi.AdaAssetId) . getChange utxos

hasDatum :: TxOut -> Bool
hasDatum = isJust . Tx.txOutDatumHash

hasNoDatum :: TxOut -> Bool
hasNoDatum = not . hasDatum

balanceTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Map TxOutRef TxOut ->
  Tx ->
  Eff effs (Either WAPI.WalletAPIError Tx)
balanceTxIns utxos tx = do
  runEitherT $ do
    let txOuts = Tx.txOutputs tx
        nonMintedValue = mconcat (map Tx.txOutValue txOuts) `ledgerMinus` txMint tx
        minSpending =
          mconcat
            [ txFee tx
            , nonMintedValue
            ]
    txIns <- newEitherT $ selectTxIns @w (Set.fromList $ txInputs tx) utxos minSpending
    -- constantly adding inputs and running balance loop forever
    pure $
      tx
        { txInputs = Set.fromList (txInputs tx) ^.. to (<> txIns) . folded
        }

-- | Set collateral
addTxCollaterals :: CollateralUtxo -> Tx -> Tx
addTxCollaterals cOut tx = tx {txCollateralInputs = [Tx.pubKeyTxInput (collateralTxOutRef cOut)]}

txUsesScripts :: Tx -> Bool
txUsesScripts Tx {txInputs, txScripts} =
  not (null txScripts) -- No provided scripts
    || any (txInputUsesRef . Tx.txInputType) txInputs -- No reference scripts
  where
    txInputUsesRef :: Ledger.TxInputType -> Bool
    txInputUsesRef (Ledger.TxScriptAddress _ (Right _) _) = True
    txInputUsesRef _ = False

-- | Ensures all non ada change goes back to user
handleNonAdaChange ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  BalanceConfig ->
  CApi.AddressInEra CApi.BabbageEra ->
  Map TxOutRef TxOut ->
  Tx ->
  Eff effs (Either WAPI.WalletAPIError Tx)
handleNonAdaChange balanceCfg changeAddr utxos tx = runEitherT $ do
  let nonAdaChange :: CApi.Value
      nonAdaChange = getNonAdaChange utxos tx

      predicate :: TxOut -> Bool
      predicate txOut =
        txOutAddress txOut == changeAddr
          && hasNoDatum txOut

      predicateChange :: TxOut -> Bool
      predicateChange =
        if bcSeparateChange balanceCfg
          then \txOut -> predicate txOut && not (justLovelace $ txOutValue txOut)
          else predicate

      newOutput :: TxOut
      newOutput =
        addressTxOut
          changeAddr
          (nonAdaChange <> lovelaceValueOf 1)

  newOutputWithMinAmt <-
    firstEitherT WAPI.OtherError $
      newEitherT $ minUtxo @w newOutput

  let outputs :: [TxOut]
      outputs =
        modifyFirst
          predicateChange
          (Just . maybe newOutputWithMinAmt (addValue nonAdaChange))
          (txOutputs tx)

  if isValueNat nonAdaChange
    then return $ if isZero nonAdaChange then tx else tx {txOutputs = outputs}
    else throwE $ WAPI.InsufficientFunds "Not enough inputs to balance tokens."

-- TODO: This function is horrible, too much repeated code

{- | `addAdaChange` checks if `bcSeparateChange` is true,
      if it is then we add the ada change to seperate `TxOut` at changeAddr that contains only ada,
      else we add it to any `TxOut` present at changeAddr.
-}
addAdaChange :: BalanceConfig -> CApi.AddressInEra CApi.BabbageEra -> Integer -> Tx -> Tx
addAdaChange _ _ 0 tx = tx
addAdaChange balanceCfg changeAddr change tx
  | bcSeparateChange balanceCfg =
    tx
      { txOutputs =
          List.reverse $
            modifyFirst
              ( \txOut ->
                  txOutAddress txOut == changeAddr
                    && justLovelace (txOutValue txOut)
                    && hasNoDatum txOut
              )
              (fmap $ addValue $ lovelaceValueOf change)
              (List.reverse $ txOutputs tx)
      }
  | otherwise =
    tx
      { txOutputs =
          modifyFirst
            (\txOut -> txOutAddress txOut == changeAddr && hasNoDatum txOut)
            (fmap $ addValue $ lovelaceValueOf change)
            (txOutputs tx)
      }

-- | creates a Tx output with min lovelace.
addOutput ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  CApi.AddressInEra CApi.BabbageEra ->
  Tx ->
  Eff effs (Either Text Tx)
addOutput changeAddr tx =
  runEitherT $ do
    let changeTxOut :: TxOut
        changeTxOut =
          addressTxOut
            changeAddr
            (lovelaceValueOf 1)

    changeTxOutWithMinAmt <-
      newEitherT $
        minUtxo @w changeTxOut

    return $ tx {txOutputs = txOutputs tx ++ [changeTxOutWithMinAmt]}

{- | Add the required signatories to the transaction. Be aware the the signature itself is invalid,
 and will be ignored. Only the pub key hashes are used, mapped to signing key files on disk.
-}
addSignatories :: PubKeyHash -> Map PubKeyHash DummyPrivKey -> [PubKeyHash] -> Tx -> Either WAPI.WalletAPIError Tx
addSignatories ownPkh privKeys pkhs tx =
  foldM
    ( \tx' pkh ->
        case Map.lookup pkh privKeys of
          Just privKey -> Right $ Tx.addSignature' (unDummyPrivateKey privKey) tx'
          Nothing -> Left $ WAPI.PaymentPrivateKeyNotFound $ PaymentPubKeyHash pkh
    )
    tx
    (ownPkh : pkhs)

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
  -- | Modifier, input Maybe representing existing value (or Nothing if missing),
  --   output value representing new value (or Nothing to remove)
  (Maybe a -> Maybe a) ->
  [a] ->
  [a]
modifyFirst _ m [] = m Nothing `consJust` []
modifyFirst p m (x : xs) = if p x then m (Just x) `consJust` xs else x : modifyFirst p m xs

minus :: CApi.Value -> CApi.Value -> CApi.Value
minus a b = a <> CApi.negateValue b

ledgerMinus :: Value -> Value -> Value
ledgerMinus x y =
  let negativeValues = map (\(c, t, a) -> (c, t, - a)) $ Value.flattenValue y
   in x <> mconcat (map unflattenValue negativeValues)

unflattenValue :: (CurrencySymbol, TokenName, Integer) -> Value
unflattenValue (curSymbol, tokenName, amount) =
  Value.assetClassValue (Value.assetClass curSymbol tokenName) amount

isValueNat :: CApi.Value -> Bool
isValueNat =
  all (\(_, q) -> q >= 0) . CApi.valueToList

isZero :: CApi.Value -> Bool
isZero = null . CApi.valueToList

justLovelace :: CApi.Value -> Bool
justLovelace (CApi.valueToList -> [(CApi.AdaAssetId, _)]) = True
justLovelace _ = False

txOutValue :: TxOut -> CApi.Value
txOutValue (TxOut (CApi.TxOut _ v _ _)) = CApi.txOutValueToValue v

txOutAddress :: TxOut -> CApi.AddressInEra CApi.BabbageEra
txOutAddress (TxOut (CApi.TxOut a _ _ _)) = a

consJust :: forall (a :: Type). Maybe a -> [a] -> [a]
consJust (Just x) = (x :)
consJust _ = id
