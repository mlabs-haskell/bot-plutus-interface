{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.Balance (tests) where

import BotPlutusInterface.Balance (balanceTxIO, defaultBalanceConfig, withFee)
import BotPlutusInterface.Balance qualified as Balance
import BotPlutusInterface.Effects (PABEffect)
import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcOwnPubKeyHash, pcProtocolParams),
 )
import Control.Lens ((&), (.~), (^.))
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut (..),
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api qualified as Api
import PlutusTx qualified
import Spec.MockContract (
  MockContractState,
  contractEnv,
  paymentPkh3,
  pkh3,
  pkhAddr3,
  -- runContractPure,
  runPABEffectPure,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
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
    , testCase "Don't add change to UTxOs with datums" dontAddChangeToDatum
    ]

validator :: Scripts.Validator
validator =
  Scripts.mkValidatorScript
    $$(PlutusTx.compile [||(\_ _ _ -> ())||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

pkh1, pkh2 :: PubKeyHash
pkh1 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 1
pkh2 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 2

addr1, addr2, valAddr :: Address
addr1 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing
addr2 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh2) Nothing
valAddr = Ledger.scriptAddress validator

txOutRef1, txOutRef2, txOutRef3, txOutRef4, txOutRef5, txOutRef6, txOutRef7 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2
txOutRef5 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 0
txOutRef6 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 3
txOutRef7 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 1

txIn1, txIn2, txIn3, txIn4, txIn5 :: TxIn
txIn1 = TxIn txOutRef1 (Just ConsumePublicKeyAddress)
txIn2 = TxIn txOutRef2 (Just ConsumePublicKeyAddress)
txIn3 = TxIn txOutRef3 (Just ConsumePublicKeyAddress)
txIn4 = TxIn txOutRef4 (Just ConsumePublicKeyAddress)
txIn5 = TxIn txOutRef5 (Just ConsumeSimpleScriptAddress)

utxo1, utxo2, utxo3, utxo4, utxo7 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, TxOut addr1 (Ada.lovelaceValueOf 1_100_000) Nothing)
utxo2 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 1_000_000) Nothing)
utxo3 = (txOutRef3, TxOut addr1 (Ada.lovelaceValueOf 900_000) Nothing)
utxo4 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "11223344" "Token" 200) Nothing)
-- utxo5 = (txOutRef5, TxOut addr3 (Ada.lovelaceValueOf 900_000) (Just $ Ledger.DatumHash ""))
-- utxo6 = (txOutRef6, TxOut addr3 (Value.singleton "11223344" "Token" 200) Nothing)
utxo7 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 5_000_000) Nothing)

scrValue :: Value.Value
scrValue = Value.singleton "11223344" "Token" 200 <> Ada.lovelaceValueOf 500_000

scrDatum :: Ledger.Datum
scrDatum = Ledger.Datum $ Api.toBuiltinData (23 :: Integer)

scrDatumHash :: Ledger.DatumHash
scrDatumHash = Ledger.datumHash scrDatum

addUtxosForFees :: Assertion
addUtxosForFees = do
  let txout = TxOut addr2 (Ada.lovelaceValueOf 1_000_000) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      minUtxo = [(txout, 1_000_000)]
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig minUtxo utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balanceTx -> txInputs <$> balanceTx @?= Right (Set.fromList [txIn1, txIn2])

addUtxosForNativeTokens :: Assertion
addUtxosForNativeTokens = do
  let txout = TxOut addr2 (Value.singleton "11223344" "Token" 123) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      minUtxo = [(txout, 1_000_000)]
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3, utxo4]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig minUtxo utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right (Set.fromList [txIn3, txIn4])

addUtxosForChange :: Assertion
addUtxosForChange = do
  let txout = TxOut addr2 (Ada.lovelaceValueOf 1_600_000) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      minUtxo = [(txout, 1_000_000)]
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig minUtxo utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right (Set.fromList [txIn1, txIn2])

dontAddChangeToDatum :: Assertion
dontAddChangeToDatum = do
  let scrTxOut' =
        ScriptChainIndexTxOut
          valAddr
          (Right validator) -- (valHash, Just validator)
          (Right scrDatum) -- (scrDatumHash, Just scrDatum)
          scrValue
      scrTxOut = Ledger.toTxOut scrTxOut'
      usrTxOut' =
        PublicKeyChainIndexTxOut
          pkhAddr3
          (Ada.lovelaceValueOf 5_000_000)
      usrTxOut = Ledger.toTxOut usrTxOut'
      -- initState :: MockContractState ()
      initState =
        def & utxos .~ [(txOutRef6, scrTxOut), (txOutRef7, usrTxOut)]
          & contractEnv .~ contractEnv'
      pabConf :: PABConfig
      pabConf = def {pcOwnPubKeyHash = pkh3}
      -- contractEnv' :: ContractEnvironment ()
      contractEnv' = def {cePABConfig = pabConf}

      scrLkups =
        Constraints.unspentOutputs (Map.fromList [(txOutRef6, scrTxOut'), (txOutRef7, usrTxOut')])
          <> Constraints.ownPaymentPubKeyHash paymentPkh3
      txConsts =
        -- Pay the same datum to the script, but with more ada.
        Constraints.mustPayToOtherScript valHash scrDatum (scrValue <> Ada.lovelaceValueOf 1_000_000)
          <> Constraints.mustSpendScriptOutput txOutRef6 Ledger.unitRedeemer
          <> Constraints.mustSpendPubKeyOutput txOutRef7
      eunbalancedTx = Constraints.mkTx @Void scrLkups txConsts

  case eunbalancedTx of
    Left mkTxErr -> assertFailure ("MkTx Error: " <> show mkTxErr)
    Right unbalancedTx -> do
      let (eRslt, _finalState) = runPABEffectPure initState (balanceTxIO @() @'[PABEffect ()] pabConf pkh3 unbalancedTx)
      case eRslt of
        (Left txt) -> assertFailure ("PAB effect error: " <> Text.unpack txt)
        (Right (Left txt)) -> assertFailure $ "Balancing error: " <> Text.unpack txt -- <> "\n(Tx: " <> show unbalancedTx <> ")"
        (Right (Right trx)) -> do
          -- TODO: Write the actual test.
          assertBool
            ( "Original UTxO not in output Tx."
                <> "\nOriginal UTxO: "
                <> show scrTxOut
                <> "\nNew UTxOs: "
                <> show (txOutputs trx)
            )
            (scrTxOut `elem` txOutputs trx)
