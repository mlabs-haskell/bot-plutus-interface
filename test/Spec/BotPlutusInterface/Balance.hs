module Spec.BotPlutusInterface.Balance (tests) where

import BotPlutusInterface.Balance (defaultBalanceConfig, withFee)
import BotPlutusInterface.Balance qualified as Balance
import BotPlutusInterface.Effects (PABEffect)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Ada qualified as Value
import Ledger.Address (Address, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Crypto (PubKeyHash)
import Ledger.Tx (Tx (..), TxIn (..), TxInType (..), TxOut (..), TxOutRef (..))
import Ledger.Value qualified as Value
import Spec.MockContract (runPABEffectPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Relude

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
    ]

pkh1, pkh2 :: PubKeyHash
pkh1 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 1
pkh2 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 2

addr1, addr2 :: Address
addr1 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing
addr2 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh2) Nothing

txOutRef1, txOutRef2, txOutRef3, txOutRef4 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2

txIn1, txIn2, txIn3, txIn4 :: TxIn
txIn1 = TxIn txOutRef1 (Just ConsumePublicKeyAddress)
txIn2 = TxIn txOutRef2 (Just ConsumePublicKeyAddress)
txIn3 = TxIn txOutRef3 (Just ConsumePublicKeyAddress)
txIn4 = TxIn txOutRef4 (Just ConsumePublicKeyAddress)

utxo1, utxo2, utxo3, utxo4 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, TxOut addr1 (Ada.lovelaceValueOf 1_100_000) Nothing)
utxo2 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 1_000_000) Nothing)
utxo3 = (txOutRef3, TxOut addr1 (Ada.lovelaceValueOf 900_000) Nothing)
utxo4 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "11223344" "Token" 200) Nothing)

addUtxosForFees :: Assertion
addUtxosForFees = do
  let txout = TxOut addr2 (Ada.lovelaceValueOf 1_000_000) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $ do
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balanceTx -> txInputs <$> balanceTx @?= Right [txIn1, txIn2]

addUtxosForNativeTokens :: Assertion
addUtxosForNativeTokens = do
  let minimumAdaRequired = Value.adaValueOf 1
      {- `minimumAdaRequired` has to be added to `txout` because
       balancing now decoupled from adjusting minimum Ada amount in output,
       and adjusting happens during `adjustUnbalancedTx` Contract
       effect execution *before* balancing. Adding `minimumAdaRequired`
       to `txout` Value aims to simulate result of `adjustUnbalancedTx` call.
       Note that 1 Ada is test value - real amount is determined by Ledger and can vary.
      -}
      txout = TxOut addr2 (Value.singleton "11223344" "Token" 123 <> minimumAdaRequired) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3, utxo4]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right [txIn3, txIn4]

addUtxosForChange :: Assertion
addUtxosForChange = do
  let txout = TxOut addr2 (Ada.lovelaceValueOf 1_600_000) Nothing
      tx = mempty {txOutputs = [txout]} `withFee` 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      ownAddr = addr1
      ebalancedTx =
        fst $
          runPABEffectPure def $
            Balance.balanceTxStep @() @'[PABEffect ()] defaultBalanceConfig utxoIndex ownAddr tx

  case ebalancedTx of
    Left e -> assertFailure (Text.unpack e)
    Right balancedTx -> txInputs <$> balancedTx @?= Right [txIn1, txIn2]
