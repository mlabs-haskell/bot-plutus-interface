{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.Contract (tests) where

import Control.Monad (void)
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (TxOutRef (TxOutRef), txOutRefId)
import Ledger.TxId (getTxId)
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), Endpoint, submitTx, submitTxConstraintsWith)
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import Spec.MockContract (
  MockContractState (..),
  addr1,
  addr2,
  pkh1',
  pkh3',
  pubKey2,
  pubKey3,
  runContractPure,
 )
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
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      initState = def {utxos = [(txOutRef, 1250, [])]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey (Ledger.pubKeyHash pubKey2) (Ada.lovelaceValueOf 1000)
        void $ submitTx constraints

      (result, state) = runContractPure contract initState
  result @?= Right ()
  state.commandHistory
    @?= map
      (Text.replace "\n" " ")
      [ [text|
            cardano-cli query utxo
            --address ${addr1}
            --mainnet
           |]
      , [text|
            cardano-cli transaction build --alonzo-era
            --tx-in ${txId}#0
            --tx-in-collateral ${txId}#0
            --tx-out ${addr2}+1000
            --change-address ${addr1}
            --required-signer signing-keys/signing-key-${pkh1'}.skey
            --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
          |]
      , [text|
            cardano-cli transaction sign
            --tx-body-file tx.raw
            --signing-key-file signing-keys/signing-key-${pkh1'}.skey
            --out-file tx.signed
          |]
      ]

multisigSupport :: Assertion
multisigSupport = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      initState = def {utxos = [(txOutRef, 1250, [])]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey (Ledger.pubKeyHash pubKey2) (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy (Ledger.pubKeyHash pubKey3)
        void $ submitTx constraints

      (result, state) = runContractPure contract initState
  -- Building and siging the tx includes both signing keys
  result @?= Right ()
  state.commandHistory
    @?= map
      (Text.replace "\n" " ")
      [ [text|
          cardano-cli query utxo
          --address ${addr1}
          --mainnet
          |]
      , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${txId}#0
          --tx-in-collateral ${txId}#0
          --tx-out ${addr2}+1000
          --change-address ${addr1}
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --required-signer signing-keys/signing-key-${pkh3'}.skey
          --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
        |]
      , [text| 
          cardano-cli transaction sign
          --tx-body-file tx.raw
          --signing-key-file signing-keys/signing-key-${pkh1'}.skey
          --signing-key-file signing-keys/signing-key-${pkh3'}.skey
          --out-file tx.signed
        |]
      ]

sendTokens :: Assertion
sendTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      initState = def {utxos = [(txOutRef, 1250, [("abcd1234.testToken", 100)])]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                (Ledger.pubKeyHash pubKey2)
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        void $ submitTx constraints

      (result, state) = runContractPure contract initState
  result @?= Right ()
  (state.commandHistory !! 1)
    @?= Text.replace
      "\n"
      " "
      [text|
        cardano-cli transaction build --alonzo-era
        --tx-in ${txId}#0
        --tx-in-collateral ${txId}#0
        --tx-out ${addr1}+50 + 95 abcd1234.testToken
        --tx-out ${addr2}+1000 + 5 abcd1234.testToken
        --change-address ${addr1}
        --required-signer signing-keys/signing-key-${pkh1'}.skey
        --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
      |]

mintTokens :: Assertion
mintTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      initState = def {utxos = [(txOutRef, 1250, [])]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      mintingPolicy :: Scripts.MintingPolicy
      mintingPolicy =
        Scripts.mkMintingPolicyScript
          $$(PlutusTx.compile [||(\_ _ -> ())||])

      curSymbol :: Value.CurrencySymbol
      curSymbol = Ledger.scriptCurrencySymbol mintingPolicy

      curSymbol' :: Text
      curSymbol' = encodeByteString $ fromBuiltin $ Value.unCurrencySymbol curSymbol

      redeemerHash =
        let (Scripts.RedeemerHash rh) = Ledger.redeemerHash Ledger.unitRedeemer
         in encodeByteString $ fromBuiltin rh

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

      (result, state) = runContractPure contract initState
  result @?= Right ()
  (state.commandHistory !! 1)
    @?= Text.replace
      "\n"
      " "
      [text| cardano-cli transaction build --alonzo-era
       --tx-in ${txId}#0
       --tx-in-collateral ${txId}#0
       --tx-out ${addr2}+1000 + 5 ${curSymbol'}.testToken
       --mint-script-file result-scripts/policy-${curSymbol'}.plutus
       --mint-redeemer-file result-scripts/redeemer-${redeemerHash}.json
       --mint 5 ${curSymbol'}.testToken
       --change-address ${addr1}
       --required-signer signing-keys/signing-key-${pkh1'}.skey
       --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
      |]
