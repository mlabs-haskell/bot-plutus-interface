{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.Contract (tests) where

import Control.Monad (void)
import Control.Monad.Freer.Error (throwError)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), Endpoint, submitTx)
import Spec.MockContract (MockConfig (..), pubKey2, pubKey3, runContractPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Prelude

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "MLabsPAB.Contracts"
    [ testCase "Send ada to address" sendAda
    , testCase "Support multiple signatories" multisigSupport
    , testCase "Support native tokens" sendTokens
    ]

sendAda :: Assertion
sendAda = do
  let mockConfig =
        MockConfig
          { handleCliCommand = \case
              ("cardano-cli", "query" : "utxo" : _) ->
                pure $
                  queryUtxoOut
                    [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250, [])]
              ("cardano-cli", "transaction" : "build" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "sign" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "submit" : _) ->
                pure ""
              _ -> throwError @Text ""
          }

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey (Ledger.pubKeyHash pubKey2) (Ada.lovelaceValueOf 1000)
        void $ submitTx constraints

      (result, state) = runContractPure contract mockConfig def
  result @?= Right ()
  state.commandHistory
    @?= [ "cardano-cli query utxo \
          \--address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35 \
          \--testnet-magic 42"
        , "cardano-cli transaction build --alonzo-era \
          \--tx-in e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
          \--tx-in-collateral e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
          \--tx-out addr_test1vqxk54m7j3q6mrkevcunryrwf4p7e68c93cjk8gzxkhlkpsffv7s0+1000 \
          \--change-address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35 \
          \--required-signer signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey \
          \--testnet-magic 42 --protocol-params-file ./protocol.json --out-file tx.raw"
        , "cardano-cli transaction sign \
          \--tx-body-file tx.raw \
          \--signing-key-file signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey \
          \--out-file tx.signed"
        ]

multisigSupport :: Assertion
multisigSupport = do
  let mockConfig =
        MockConfig
          { handleCliCommand = \case
              ("cardano-cli", "query" : "utxo" : _) ->
                pure $
                  queryUtxoOut
                    [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250, [])]
              ("cardano-cli", "transaction" : "build" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "sign" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "submit" : _) ->
                pure ""
              _ -> throwError @Text ""
          }

      contract :: Contract Text (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey (Ledger.pubKeyHash pubKey2) (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy (Ledger.pubKeyHash pubKey3)
        void $ submitTx constraints

      (result, state) = runContractPure contract mockConfig def
  -- Building and siging the tx includes both signing keys
  result @?= Right ()
  state.commandHistory
    @?= [ "cardano-cli query utxo \
          \--address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35 \
          \--testnet-magic 42"
        , "cardano-cli transaction build --alonzo-era \
          \--tx-in e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
          \--tx-in-collateral e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
          \--tx-out addr_test1vqxk54m7j3q6mrkevcunryrwf4p7e68c93cjk8gzxkhlkpsffv7s0+1000 \
          \--change-address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35 \
          \--required-signer signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey \
          \--required-signer signing-keys/signing-key-008b47844d92812fc30d1f0ac9b6fbf38778ccba9db8312ad9079079.skey \
          \--testnet-magic 42 --protocol-params-file ./protocol.json --out-file tx.raw"
        , "cardano-cli transaction sign \
          \--tx-body-file tx.raw \
          \--signing-key-file signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey \
          \--signing-key-file signing-keys/signing-key-008b47844d92812fc30d1f0ac9b6fbf38778ccba9db8312ad9079079.skey \
          \--out-file tx.signed"
        ]

sendTokens :: Assertion
sendTokens = do
  let mockConfig =
        MockConfig
          { handleCliCommand = \case
              ("cardano-cli", "query" : "utxo" : _) ->
                pure $
                  queryUtxoOut
                    [
                      ( "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5"
                      , 0
                      , 1250
                      , [("abcd1234.testToken", 100)]
                      )
                    ]
              ("cardano-cli", "transaction" : "build" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "sign" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "submit" : _) ->
                pure ""
              _ -> throwError @Text ""
          }

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                (Ledger.pubKeyHash pubKey2)
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        void $ submitTx constraints

      (result, state) = runContractPure contract mockConfig def
  result @?= Right ()
  (state.commandHistory !! 1)
    @?= "cardano-cli transaction build --alonzo-era \
        \--tx-in e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
        \--tx-in-collateral e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0 \
        \--tx-out addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35+50 + 95 abcd1234.testToken \
        \--tx-out addr_test1vqxk54m7j3q6mrkevcunryrwf4p7e68c93cjk8gzxkhlkpsffv7s0+1000 + 5 abcd1234.testToken \
        \--change-address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35 \
        \--required-signer signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey \
        \--testnet-magic 42 --protocol-params-file ./protocol.json --out-file tx.raw"

queryUtxoOut :: [(Text, Int, Int, [(Text, Int)])] -> String
queryUtxoOut utxos =
  Text.unpack $
    Text.unlines
      [ "                           TxHash                                 TxIx        Amount"
      , "--------------------------------------------------------------------------------------"
      , Text.unlines $
          map
            ( \(txId, txIx, amt, tokens) ->
                let txIx' = Text.pack $ show txIx
                    amts =
                      Text.intercalate
                        " + "
                        ( Text.pack (show amt) <> " " <> "lovelace" :
                          map (\(tSymbol, tAmt) -> Text.pack (show tAmt) <> " " <> tSymbol) tokens
                        )
                 in [text|${txId}     ${txIx'}        ${amts} + TxOutDatumNone"|]
            )
            utxos
      ]
