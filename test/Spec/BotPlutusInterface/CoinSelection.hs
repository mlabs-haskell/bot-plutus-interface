module Spec.BotPlutusInterface.CoinSelection (tests) where

import BotPlutusInterface.CoinSelection (uniqueAssetClasses)
import Control.Lens (folded, (^..), to)
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Crypto (PubKeyHash)
import Ledger.Tx (Tx (..), TxIn (..), TxInType (..), TxOut (..), TxOutRef (..))
import Ledger.Value qualified as Value
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude


tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.CoinSelection"
    [ testCase "validAssetClasses" validAssetClasses
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
txOutRef5 = TxOutRef "34d491e0596b3a04be6e3442ebf115b33900f26e5e415e5151f820778ba576ed" 0

utxo1, utxo2, utxo3, utxo4 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, TxOut addr1 (Ada.lovelaceValueOf 1_100_000) Nothing)
utxo2 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 1_000_000) Nothing)
utxo3 = (txOutRef3, TxOut addr1 (Ada.lovelaceValueOf 900_000) Nothing)
utxo4 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "11223344" "Token1" 500) Nothing)
utxo5 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "44332211" "Token2" 500) Nothing)


validAssetClasses :: Assertion
validAssetClasses = (uniqueAssetClasses $ map (txOutValue . snd) [utxo1,utxo2,utxo3,utxo4,utxo5]) @?= Set.fromList
  [Value.assetClass Ada.adaSymbol Ada.adaToken, Value.assetClass "44332211" "Token2", Value.assetClass "11223344" "Token1"]
