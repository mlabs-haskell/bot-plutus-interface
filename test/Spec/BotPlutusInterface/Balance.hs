{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.BotPlutusInterface.Balance (tests) where

import BotPlutusInterface.Balance (balanceTxIO, defaultBalanceConfig, withFee)
import BotPlutusInterface.Balance qualified as Balance
import BotPlutusInterface.Effects (PABEffect)
import BotPlutusInterface.Helpers (addressTxOut, lovelaceValueOf, unsafeToCardanoAddressInEra, unsafeValueOf)
import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcOwnPubKeyHash),
 )
import Cardano.Api (AddressInEra, BabbageEra)
import Control.Lens ((&), (.~), (^.))
import Data.Default (Default (def))
import Data.Either.Combinators (fromRight)
import Data.Function (on)
import Data.List (delete, partition)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain qualified as OffChain
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut (..),
  Tx (..),
  TxInput (..),
  TxInputType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Value qualified as Value

import Ledger.Value (AssetClass, Value)
import Plutus.Script.Utils.Scripts qualified as ScriptUtils
import Plutus.Script.Utils.V1.Address qualified as ScriptUtils
import Plutus.V1.Ledger.Api qualified as Api
import PlutusTx qualified
import Prettyprinter (pretty)
import Spec.MockContract (
  MockContractState,
  contractEnv,
  currencySymbol1,
  paymentPkh3,
  pkh3,
  pkhAddr3,
  runPABEffectPure,
  testingNetwork,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Text.Printf (printf)
import Prelude

{- | Tests for 'cardano-cli query utxo' result parsers
 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.Balance"
    [ testCase "Add utxos to cover fees" addUtxosForFees
    , testCase "Add utxos to cover native tokens" addUtxosForNativeTokens
    , testCase "Add utxos to cover change min utxo" addUtxosForChange
    , testCase "Don't add change to UTxOs with datums (1)" dontAddChangeToDatum
    , testCase "Don't add change to UTxOs with datums (2)" dontAddChangeToDatum2
    ]

validator :: Scripts.Validator
validator =
  Scripts.mkValidatorScript
    $$(PlutusTx.compile [||(\_ _ _ -> ())||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash $ toV1 validator

pkh1, pkh2 :: PubKeyHash
pkh1 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 1
pkh2 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 2

addr1, addr2 :: AddressInEra BabbageEra
addr1 = unsafeToCardanoAddressInEra testingNetwork $ Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing
addr2 = unsafeToCardanoAddressInEra testingNetwork $ Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh2) Nothing

valAddr :: Address
valAddr = ScriptUtils.mkValidatorAddress validator

txOutRef1, txOutRef2, txOutRef3, txOutRef4, txOutRef5, txOutRef6, txOutRef7 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2
txOutRef5 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 0
txOutRef6 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 3
txOutRef7 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 1

txInput1, txInput2, txInput3, txInput4 :: TxInput
txInput1 = TxInput txOutRef1 TxConsumePublicKeyAddress
txInput2 = TxInput txOutRef2 TxConsumePublicKeyAddress
txInput3 = TxInput txOutRef3 TxConsumePublicKeyAddress
txInput4 = TxInput txOutRef4 TxConsumePublicKeyAddress

utxo1, utxo2, utxo3, utxo4 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, addressTxOut addr1 $ lovelaceValueOf 1_100_000)
utxo2 = (txOutRef2, addressTxOut addr1 $ lovelaceValueOf 1_000_000)
utxo3 = (txOutRef3, addressTxOut addr1 $ lovelaceValueOf 900_000)
utxo4 = (txOutRef4, addressTxOut addr1 $ lovelaceValueOf 800_000 <> unsafeValueOf currencySymbol1 "Token" 200)

-- Ada values set  to amount that covers min Ada so we don't need to deal with
-- output's adjustments
scrValue :: Value.Value
scrValue = Value.assetClassValue tokenAsset 200 <> Ada.lovelaceValueOf 2_000_000

scrDatum :: Ledger.Datum
scrDatum = Ledger.Datum $ Api.toBuiltinData (23 :: Integer)

scrDatumHash :: Ledger.DatumHash
scrDatumHash = ScriptUtils.datumHash scrDatum

acValueOf :: AssetClass -> Value -> Integer
acValueOf = flip Value.assetClassValueOf

-- | Get the amount of lovelace in a `Value`.
lovelaceInValue :: Value -> Integer
lovelaceInValue = acValueOf (Value.assetClass Api.adaSymbol Api.adaToken)

tokenAsset :: Value.AssetClass
tokenAsset = Value.assetClass currencySymbol1 "Token"

unsafeToTxOut :: ChainIndexTxOut -> TxOut
unsafeToTxOut = fromRight undefined . Ledger.toTxOut testingNetwork

addUtxosForFees :: Assertion
addUtxosForFees = do
  let txOut = addressTxOut addr2 (lovelaceValueOf 1_000_000)
      tx = mempty {txOutputs = [txOut]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $ do
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balanceTx -> txInputs <$> balanceTx @?= Right [txInput1, txInput2]

addUtxosForNativeTokens :: Assertion
addUtxosForNativeTokens = do
  let txOut = addressTxOut addr2 (unsafeValueOf currencySymbol1 "Token" 123)
      tx = mempty {txOutputs = [txOut]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3, utxo4]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right [txInput4]

addUtxosForChange :: Assertion
addUtxosForChange = do
  let txOut = addressTxOut addr2 (lovelaceValueOf 1_600_000)
      tx = mempty {txOutputs = [txOut]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right [txInput1, txInput2]

dontAddChangeToDatum :: Assertion
dontAddChangeToDatum = do
  let scrTxOut =
        ScriptChainIndexTxOut
          valAddr
          scrValue
          (toHashAndDatum scrDatum)
          Nothing
          (toHashAndValidator validator)
      -- scrTxOut = Ledger.toTxOut scrTxOut'
      usrTxOut =
        PublicKeyChainIndexTxOut
          pkhAddr3
          (Ada.lovelaceValueOf 1_001_000)
          Nothing
          Nothing
      -- usrTxOut = Ledger.toTxOut usrTxOut'
      initState :: MockContractState ()
      initState =
        def & utxos .~ [(txOutRef6, scrTxOut), (txOutRef7, usrTxOut)]
          & contractEnv .~ contractEnv'
      pabConf :: PABConfig
      pabConf = def {pcOwnPubKeyHash = pkh3}
      contractEnv' :: ContractEnvironment ()
      contractEnv' = def {cePABConfig = pabConf}

      -- Input UTxOs:
      -- UTxO 1:
      -- - From: User
      -- - Amt : 1.001 ADA
      -- UTxO 2:
      -- - From: Script
      -- - Amt : 2 ADA + 200 Tokens
      --
      -- Output UTxOs:
      -- UTxO 1:
      -- - To : User
      -- - Amt: 1 ADA
      -- UTxO 2:
      -- - To : Script
      -- - Amt: 1.5 Ada + 200 Token
      --
      -- Fees   : 400 Lovelace
      -- Change : 500600 Lovelace

      scrLkups =
        Constraints.unspentOutputs (Map.fromList [(txOutRef6, scrTxOut), (txOutRef7, usrTxOut)])
          <> Constraints.ownPaymentPubKeyHash paymentPkh3
          <> Constraints.plutusV1OtherScript validator

      payToScriptValue = Ada.lovelaceValueOf 1_500_000
      payToUserValue = Ada.lovelaceValueOf 1_000_000
      txConsts =
        -- Pay the same datum to the script, but with more ada.
        Constraints.mustPayToOtherScript valHash scrDatum payToScriptValue
          <> Constraints.mustPayToPubKey paymentPkh3 payToUserValue
          <> Constraints.mustSpendScriptOutput txOutRef6 Ledger.unitRedeemer
          <> Constraints.mustSpendPubKeyOutput txOutRef7
      eunbalancedTx = Constraints.mkTx @Void scrLkups txConsts

  unbalancedTx <- liftAssertFailure eunbalancedTx (\err -> "MkTx Error: " <> show err)
  let (eRslt, _finalState) = runPABEffectPure initState (balanceTxIO @() @'[PABEffect ()] pabConf pkh3 unbalancedTx)
  eRslt' <- liftAssertFailure eRslt (\txt -> "PAB effect error: " <> show txt)
  trx <- liftAssertFailure eRslt' (\txt -> "Balancing error: " <> show txt)
  let scrTxOut'' = scrTxOut & Ledger.ciTxOutValue .~ payToScriptValue
      scrTxOutExpected = unsafeToTxOut scrTxOut''
      isScrUtxo :: TxOut -> Bool
      isScrUtxo utxo = Ledger.txOutAddress utxo == Ledger.txOutAddress scrTxOutExpected
      (balScrUtxos, balOtherUtxos) = partition isScrUtxo (txOutputs trx)
  assertBool
    ( "Expected UTxO not in output Tx."
        <> "\nExpected UTxO: \n"
        <> show (pretty scrTxOutExpected)
        <> "\nBalanced Script UTxOs: \n"
        <> show (pretty balScrUtxos)
        <> "\nOther Balanced UTxOs: \n"
        <> show (pretty balOtherUtxos)
        <> "\nUnbalanced UTxOs: \n"
        <> show (pretty (txOutputs (unbalancedTx ^. OffChain.tx)))
    )
    (scrTxOutExpected `elem` txOutputs trx)

-- Like the first one, but
-- only has inputs from the script.
dontAddChangeToDatum2 :: Assertion
dontAddChangeToDatum2 = do
  let scrTxOut =
        ScriptChainIndexTxOut
          valAddr
          (scrValue <> Ada.lovelaceValueOf 1_500_000)
          (toHashAndDatum scrDatum)
          Nothing
          (toHashAndValidator validator)
      -- scrTxOut = Ledger.toTxOut scrTxOut'
      initState :: MockContractState ()
      initState =
        def & utxos .~ [(txOutRef6, scrTxOut)]
          & contractEnv .~ contractEnv'
      pabConf :: PABConfig
      pabConf = def {pcOwnPubKeyHash = pkh3}
      contractEnv' :: ContractEnvironment ()
      contractEnv' = def {cePABConfig = pabConf}

      -- Input UTxO :
      -- - 3.5 ADA
      -- - 200 tokens
      -- Output UTxO :
      -- - 2 ADA
      -- - 120 tokens
      -- Change:
      -- - 1.5 ADA (400 Lovelace to fees)
      -- - 80 tokens

      payToScrValue :: Value.Value
      payToScrValue = Value.assetClassValue tokenAsset 120 <> Ada.lovelaceValueOf 2_000_000

      scrLkups =
        Constraints.unspentOutputs (Map.fromList [(txOutRef6, scrTxOut)])
          <> Constraints.ownPaymentPubKeyHash paymentPkh3
          <> Constraints.plutusV1OtherScript validator
      txConsts =
        -- Pay the same datum to the script, but with LESS ada
        -- and fewer tokens. This is to ensure that the excess
        -- ADA and tokens are moved into their own UTxO(s),
        -- rather than just being left in the original UTxO.
        -- (The extra ada is used to cover fees etc...)
        Constraints.mustPayToOtherScript valHash scrDatum payToScrValue
          <> Constraints.mustSpendScriptOutput txOutRef6 Ledger.unitRedeemer
      eunbalancedTx = Constraints.mkTx @Void scrLkups txConsts

  unbalancedTx <- liftAssertFailure eunbalancedTx (\err -> "MkTx Error: " <> show err)
  let (eRslt, _finalState) = runPABEffectPure initState (balanceTxIO @() @'[PABEffect ()] pabConf pkh3 unbalancedTx)
  eRslt' <- liftAssertFailure eRslt (\txt -> "PAB effect error: " <> show txt)
  trx <- liftAssertFailure eRslt' (\txt -> "Balancing error: " <> show txt)
  let scrTxOut'' = scrTxOut & Ledger.ciTxOutValue .~ payToScrValue
      scrTxOutExpected = unsafeToTxOut scrTxOut''
      isScrUtxo :: TxOut -> Bool
      isScrUtxo utxo = Ledger.txOutAddress utxo == Ledger.txOutAddress scrTxOutExpected
      (balScrUtxos, balOtherUtxos) = partition isScrUtxo (txOutputs trx)
  -- Check that the expected script UTxO
  -- is in the output.
  assertBool
    ( "Expected UTxO not in output Tx."
        <> "\nExpected UTxO: \n"
        <> show scrTxOutExpected
        <> "\nBalanced Script UTxOs: \n"
        <> show balScrUtxos
        <> "\nOther Balanced UTxOs: \n"
        <> show balOtherUtxos
        <> "\nUnbalanced UTxOs: \n"
        <> show (txOutputs (unbalancedTx ^. OffChain.tx))
    )
    (scrTxOutExpected `elem` txOutputs trx)
  -- Check that the output has the remaining change
  let trxFee = txFee trx
      adaChange' :: Integer
      adaChange' = ((-) `on` (lovelaceInValue . Ledger.txOutValue)) (unsafeToTxOut scrTxOut) scrTxOutExpected
      adaChange :: Integer
      adaChange = adaChange' - lovelaceInValue trxFee
      tokChange :: Integer
      tokChange = ((-) `on` (acValueOf tokenAsset . Ledger.txOutValue)) (unsafeToTxOut scrTxOut) scrTxOutExpected
      remainingTxOuts :: [TxOut]
      remainingTxOuts = delete scrTxOutExpected (txOutputs trx)
      remainingValue :: Value.Value
      remainingValue = foldMap Ledger.txOutValue remainingTxOuts
  -- Check for ADA change
  assertBool
    ( "Other UTxOs do not contain expected ADA change."
        <> printf "\nExpected Amount : %d Lovelace" adaChange
        <> printf "\nActual Amount   : %d Lovelace" (lovelaceInValue remainingValue)
    )
    (adaChange == lovelaceInValue remainingValue)
  -- Check for Token change
  assertBool
    ( "Other UTxOs do not contain expected Token change."
        <> printf "\nExpected Amount : %d tokens" tokChange
        <> printf "\nActual Amount   : %d tokens" (acValueOf tokenAsset remainingValue)
    )
    (tokChange == acValueOf tokenAsset remainingValue)

-- | Lift an `Either` value into an `assertFailure`.
liftAssertFailure :: Either a b -> (a -> String) -> IO b
liftAssertFailure (Left err) fstr = assertFailure (fstr err)
liftAssertFailure (Right rslt) _ = return rslt

toHashAndDatum :: ScriptUtils.Datum -> (ScriptUtils.DatumHash, Maybe ScriptUtils.Datum)
toHashAndDatum d = (ScriptUtils.datumHash d, Just d)

toHashAndValidator :: Api.Validator -> (Api.ValidatorHash, Maybe (ScriptUtils.Versioned Api.Validator))
toHashAndValidator (toV1 -> v) = (Scripts.validatorHash v, Just v)

toV1 :: Api.Validator -> ScriptUtils.Versioned Api.Validator
toV1 = flip ScriptUtils.Versioned ScriptUtils.PlutusV1
