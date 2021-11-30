{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.PreBalance (tests) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address)
import Ledger.Crypto (PubKeyHash, privateKey1)
import Ledger.Tx (Tx (..), TxIn (..), TxInType (..), TxOut (..), TxOutRef (..))
import Ledger.Value qualified as Value
import MLabsPAB.PreBalance qualified as PreBalance
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Wallet (defaultSlotRange)
import Wallet.Emulator.Types qualified as Wallet
import Prelude

{- | Tests for 'cardano-cli query utxo' result parsers

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "MLabsPAB.UtxoParser"
    [ testCase "Add utxos to cover fees" addUtxosForFees
    , testCase "Add utxos to cover native tokens" addUtxosForNativeTokens
    , testProperty "Double pre balancing does not change the result" prop_DoublePreBalancing
    ]

pkh1, pkh2 :: PubKeyHash
pkh1 = Ledger.pubKeyHash $ Wallet.walletPubKey (Wallet.knownWallet 1)
pkh2 = Ledger.pubKeyHash $ Wallet.walletPubKey (Wallet.knownWallet 2)

addr1, addr2 :: Address
addr1 = Ledger.pubKeyHashAddress pkh1
addr2 = Ledger.pubKeyHashAddress pkh2

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
  let tx =
        Tx
          { txInputs = mempty
          , txCollateral = mempty
          , txOutputs = [TxOut addr2 (Ada.lovelaceValueOf 1_000_000) Nothing]
          , txMint = mempty
          , txFee = mempty
          , txValidRange = defaultSlotRange
          , txMintScripts = mempty
          , txSignatures = mempty
          , txRedeemers = mempty
          , txData = mempty
          }
  let minUtxo = 1_000_000
      fees = 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3]
      privKeys = Map.fromList [(pkh1, privateKey1)]
      requiredSigs = [pkh1]
      ownPkh = pkh1
      prebalancedTx = PreBalance.preBalanceTx minUtxo fees utxoIndex ownPkh privKeys requiredSigs tx

  txInputs <$> prebalancedTx @?= Right (Set.fromList [txIn1, txIn2])

addUtxosForNativeTokens :: Assertion
addUtxosForNativeTokens = do
  let tx =
        Tx
          { txInputs = mempty
          , txCollateral = mempty
          , txOutputs = [TxOut addr2 (Value.singleton "11223344" "Token" 123) Nothing]
          , txMint = mempty
          , txFee = mempty
          , txValidRange = defaultSlotRange
          , txMintScripts = mempty
          , txSignatures = mempty
          , txRedeemers = mempty
          , txData = mempty
          }
  let minUtxo = 1_000_000
      fees = 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3, utxo4]
      privKeys = Map.fromList [(pkh1, privateKey1)]
      requiredSigs = [pkh1]
      ownPkh = pkh1
      prebalancedTx = PreBalance.preBalanceTx minUtxo fees utxoIndex ownPkh privKeys requiredSigs tx

  txInputs <$> prebalancedTx @?= Right (Set.fromList [txIn1, txIn2, txIn3, txIn4])

prop_DoublePreBalancing :: Tx -> Property
prop_DoublePreBalancing tx =
  let minUtxo = 1_000_000
      fees = 500_000
      utxoIndex = Map.fromList [utxo1, utxo2, utxo3, utxo4]
      privKeys = Map.fromList [(pkh1, privateKey1)]
      requiredSigs = [pkh1]
      ownPkh = pkh1
   in PreBalance.preBalanceTx minUtxo fees utxoIndex ownPkh privKeys requiredSigs tx
        === ( PreBalance.preBalanceTx minUtxo fees utxoIndex ownPkh privKeys requiredSigs tx
                >>= PreBalance.preBalanceTx minUtxo fees utxoIndex ownPkh privKeys requiredSigs
            )
