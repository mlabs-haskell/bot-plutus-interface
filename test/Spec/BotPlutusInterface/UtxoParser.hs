{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.UtxoParser (tests) where

import BotPlutusInterface.UtxoParser qualified as UtxoParser
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address)
import Ledger.Tx (
  ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut),
  TxOutRef (TxOutRef),
 )
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Prelude

pubKeyHashAddress :: Ledger.PubKeyHash -> Address
pubKeyHashAddress pkh = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash pkh) Nothing

{- | Tests for 'cardano-cli query utxo' result parsers

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.UtxoParser"
    [ testCase "Without utxo" withoutUtxo
    , testCase "Single utxo, ada only" singleAdaOnly
    , testCase "Multiple utxos, ada only" multiAdaOnly
    , testCase "Single utxo, ada and native tokens" singleWithNativeTokens
    , testCase "Single utxo, with datum" singleWithDatum
    ]

withoutUtxo :: Assertion
withoutUtxo = do
  let addr = pubKeyHashAddress "0000"
  testUtxoParser
    addr
    [text|                           TxHash                                 TxIx        Amount
          --------------------------------------------------------------------------------------
    |]
    []

singleAdaOnly :: Assertion
singleAdaOnly = do
  let addr = pubKeyHashAddress "0000"
  testUtxoParser
    addr
    [text|                           TxHash                                 TxIx        Amount
          --------------------------------------------------------------------------------------
          384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51     0        5000000000 lovelace + TxOutDatumNone
    |]
    [
      ( TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
      , PublicKeyChainIndexTxOut addr (Ada.lovelaceValueOf 5000000000)
      )
    ]

multiAdaOnly :: Assertion
multiAdaOnly = do
  let addr = pubKeyHashAddress "0000"
  testUtxoParser
    addr
    [text|                           TxHash                                 TxIx        Amount
          --------------------------------------------------------------------------------------
          384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51     0        5000000000 lovelace + TxOutDatumNone
          52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff     1        89835907 lovelace + TxOutDatumNone
          d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3     0        501000123456 lovelace + TxOutDatumNone
    |]
    [
      ( TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
      , PublicKeyChainIndexTxOut addr (Ada.lovelaceValueOf 5000000000)
      )
    ,
      ( TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
      , PublicKeyChainIndexTxOut addr (Ada.lovelaceValueOf 89835907)
      )
    ,
      ( TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
      , PublicKeyChainIndexTxOut addr (Ada.lovelaceValueOf 501000123456)
      )
    ]

singleWithNativeTokens :: Assertion
singleWithNativeTokens = do
  let addr = pubKeyHashAddress "0000"
  testUtxoParser
    addr
    [text|                           TxHash                                 TxIx        Amount
          --------------------------------------------------------------------------------------
          384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51     0        1234 lovelace + 2345 057910a2c93551443cb2c0544d1d65da3fb033deaa79452bd431ee08.testToken + 3456 7c6de14062b27c3dc3ba9f232ade32efe22fb8e2ae76b24f33212fdb.testToken2 + 4567 98a759ed2e20f6d83aa4d37d028d4bbb547a696fc345d54126188614 + TxOutDatumNone
    |]
    [
      ( TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
      , PublicKeyChainIndexTxOut
          addr
          ( Ada.lovelaceValueOf 1234
              <> Value.singleton "057910a2c93551443cb2c0544d1d65da3fb033deaa79452bd431ee08" "testToken" 2345
              <> Value.singleton "7c6de14062b27c3dc3ba9f232ade32efe22fb8e2ae76b24f33212fdb" "testToken2" 3456
              <> Value.singleton "98a759ed2e20f6d83aa4d37d028d4bbb547a696fc345d54126188614" "" 4567
          )
      )
    ]

singleWithDatum :: Assertion
singleWithDatum = do
  let addr = Ledger.scriptHashAddress "0000"
  testUtxoParser
    addr
    [text|                           TxHash                                 TxIx        Amount
          --------------------------------------------------------------------------------------
          384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51     0        5000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0"
    |]
    [
      ( TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
      , ScriptChainIndexTxOut
          addr
          (Left "0000")
          (Left "2cdb268baecefad822e5712f9e690e1787f186f5c84c343ffdc060b21f0241e0")
          (Ada.lovelaceValueOf 5000000000)
      )
    ]

testUtxoParser :: Address -> Text -> [(TxOutRef, ChainIndexTxOut)] -> Assertion
testUtxoParser addr output expected =
  parseOnly (UtxoParser.utxoMapParser addr) output @?= Right expected
