{-# OPTIONS_GHC -Wno-unused-binds #-}

module Spec.BotPlutusInterface.CoinSelection (tests) where

import BotPlutusInterface.CoinSelection (selectTxIns, uniqueAssetClasses)
import BotPlutusInterface.Effects (PABEffect)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Crypto (PubKeyHash)
import Ledger.Tx (TxIn (..), TxInType (..), TxOut (..), TxOutRef (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Spec.MockContract (runPABEffectPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.CoinSelection"
    [ testCase "Have All unique assetClasses" validAssetClasses
    , testCase "Coin selection greedy Approx" greedyApprox
    ]

pkh1 :: PubKeyHash
pkh1 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 1

addr1 :: Address
addr1 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing

txOutRef1, txOutRef2, txOutRef3, txOutRef4, txOutRef5 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2
txOutRef5 = TxOutRef "34d491e0596b3a04be6e3442ebf115b33900f26e5e415e5151f820778ba576ed" 0

utxo1, utxo2, utxo3, utxo4, utxo5 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, TxOut addr1 (Ada.lovelaceValueOf 1_400_000) Nothing)
utxo2 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 1_200_000) Nothing)
utxo3 = (txOutRef3, TxOut addr1 (Ada.lovelaceValueOf 900_000) Nothing)
utxo4 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "11223344" "Token1" 500) Nothing)
utxo5 = (txOutRef5, TxOut addr1 (Ada.lovelaceValueOf 600_000 <> Value.singleton "44332211" "Token2" 500) Nothing)

txIn1, txIn2, txIn3, txIn4, txIn5 :: TxIn
txIn1 = TxIn txOutRef1 (Just ConsumePublicKeyAddress)
txIn2 = TxIn txOutRef2 (Just ConsumePublicKeyAddress)
txIn3 = TxIn txOutRef3 (Just ConsumePublicKeyAddress)
txIn4 = TxIn txOutRef4 (Just ConsumePublicKeyAddress)
txIn5 = TxIn txOutRef5 (Just ConsumePublicKeyAddress)

validAssetClasses :: Assertion
validAssetClasses =
  uniqueAssetClasses (map (txOutValue . snd) [utxo1, utxo2, utxo3, utxo4, utxo5])
    @?= Set.fromList
      [ Value.assetClass Ada.adaSymbol Ada.adaToken
      , Value.assetClass "44332211" "Token2"
      , Value.assetClass "11223344" "Token1"
      ]

testOutputValue1, testOutputValue2, testOutputValue3, testOutputValue4 :: Value
testOutputValue1 = Ada.lovelaceValueOf 1_000_000 <> Value.singleton "11223344" "Token1" 100
testOutputValue2 = Ada.lovelaceValueOf 1_000_000
testOutputValue3 =
  Ada.lovelaceValueOf 1_000_000 <> Value.singleton "11223344" "Token1" 100
    <> Value.singleton "44332211" "Token2" 50
testOutputValue4 = Ada.lovelaceValueOf 1_500_000

greedyApprox :: Assertion
greedyApprox = do
  let eresult =
        mapM
          ( fst
              . runPABEffectPure def
              . selectTxIns @() @'[PABEffect ()] mempty (Map.fromList [utxo1, utxo2, utxo3, utxo4, utxo5])
          )
          [testOutputValue1, testOutputValue2, testOutputValue3, testOutputValue4]

      expectedResults =
        map
          (Right . Set.fromList)
          [ [txIn2, txIn4]
          , [txIn2]
          , [txIn4, txIn5, txIn1]
          , [txIn1, txIn2]
          ]

  case eresult of
    Left e -> assertFailure (Text.unpack e)
    Right result -> result @?= expectedResults


