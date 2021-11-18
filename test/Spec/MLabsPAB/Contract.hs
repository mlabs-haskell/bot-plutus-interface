{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.Contract (tests) where

import Control.Monad (void)
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), Endpoint, submitTx, submitTxConstraintsWith)
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import Spec.MockContract (pubKey2, pubKey3, runContractPure, withUtxos)
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
    , testCase "Send native tokens" sendTokens
    , testCase "Mint native tokens" mintTokens
    ]

sendAda :: Assertion
sendAda = do
  let mockConfig =
        def
          & withUtxos
            [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250, [])]
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
        def
          & withUtxos
            [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250, [])]
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
        def
          & withUtxos
            [
              ( "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5"
              , 0
              , 1250
              , [("abcd1234.testToken", 100)]
              )
            ]
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
    @?= Text.replace
      "\n"
      " "
      [text|
        cardano-cli transaction build --alonzo-era
        --tx-in e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0
        --tx-in-collateral e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0
        --tx-out addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35+50 + 95 abcd1234.testToken
        --tx-out addr_test1vqxk54m7j3q6mrkevcunryrwf4p7e68c93cjk8gzxkhlkpsffv7s0+1000 + 5 abcd1234.testToken
        --change-address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35
        --required-signer signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey
        --testnet-magic 42 --protocol-params-file ./protocol.json --out-file tx.raw
      |]

mintTokens :: Assertion
mintTokens = do
  let mockConfig =
        def
          & withUtxos
            [("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5", 0, 1250, [])]

      mintingPolicy :: Scripts.MintingPolicy
      mintingPolicy =
        Scripts.mkMintingPolicyScript
          $$(PlutusTx.compile [||(\_ _ -> ())||])

      curSymbol :: Value.CurrencySymbol
      curSymbol = Ledger.scriptCurrencySymbol mintingPolicy

      curSymbol' :: Text
      curSymbol' = encodeByteString $ fromBuiltin $ Value.unCurrencySymbol curSymbol

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let lookups =
              Constraints.mintingPolicy mintingPolicy
        let constraints =
              Constraints.mustMintValue (Value.singleton curSymbol "testToken" 5)
                <> Constraints.mustPayToPubKey
                  (Ledger.pubKeyHash pubKey2)
                  (Ada.lovelaceValueOf 1000 <> Value.singleton curSymbol "testToken" 5)
        void $ submitTxConstraintsWith @Void lookups constraints

      (result, state) = runContractPure contract mockConfig def
  result @?= Right ()
  (state.commandHistory !! 1)
    @?= Text.replace
      "\n"
      " "
      [text| cardano-cli transaction build --alonzo-era
       --tx-in e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0
       --tx-in-collateral e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5#0
       --tx-out addr_test1vqxk54m7j3q6mrkevcunryrwf4p7e68c93cjk8gzxkhlkpsffv7s0+1000 + 5 ${curSymbol'}.testToken
       --mint-script-file result-scripts/policy-${curSymbol'}.plutus
       --mint-redeemer-file result-scripts/redeemer-923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec.json
       --mint 5 ${curSymbol'}.testToken
       --change-address addr_test1vr9exkzjnh6898pjg632qv7tnqs6h073dhjg3qq9jp9tcsg8d6n35
       --required-signer signing-keys/signing-key-cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc41.skey
       --testnet-magic 42 --protocol-params-file ./protocol.json --out-file tx.raw
      |]
