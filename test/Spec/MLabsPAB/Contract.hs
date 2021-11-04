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
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), ContractError, Endpoint, submitTx)
import Spec.Mock (MockConfig (..), pubKey2, runContractPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))
import Prelude

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "MLabsPAB.Contracts"
    [testCase "Send ada to address" sendAda]

sendAda :: Assertion
sendAda =
  let mockConfig =
        MockConfig
          { handleCliCommand = \case
              ("cardano-cli", "query" : "utxo" : _) ->
                pure $
                  queryUtxoOut
                    [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250)]
              ("cardano-cli", "transaction" : "build" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "sign" : _) ->
                pure ""
              ("cardano-cli", "transaction" : "submit" : _) ->
                pure ""
              _ -> throwError ()
          }

      contract :: Contract () (Endpoint "SendAda" ()) ContractError ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey (Ledger.pubKeyHash pubKey2) (Ada.lovelaceValueOf 1000)
        void $ submitTx constraints

      contractResult = runContractPure contract mockConfig def
   in [ "cardano-cli query utxo \
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
        @=? reverse contractResult.commandHistory

queryUtxoOut :: [(Text, Int, Int)] -> String
queryUtxoOut utxos =
  Text.unpack $
    Text.unlines
      [ "                           TxHash                                 TxIx        Amount"
      , "--------------------------------------------------------------------------------------"
      , Text.unlines $
          map
            ( \(txId, txIx, amt) ->
                let txIx' = Text.pack $ show txIx
                    amt' = Text.pack $ show amt
                 in [text|${txId}     ${txIx'}        ${amt'} lovelace + TxOutDatumNone"|]
            )
            utxos
      ]
