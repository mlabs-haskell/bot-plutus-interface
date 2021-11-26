{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.Contract (tests) where

import Control.Monad (void)
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (TxOut (TxOut), TxOutRef (TxOutRef), txOutRefId)
import Ledger.TxId (getTxId)
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), Endpoint, submitTx, submitTxConstraintsWith, utxosAt)
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import Spec.MockContract (
  MockContractState (..),
  addr1,
  addr2,
  pkh1,
  pkh1',
  pkh2,
  pkh3,
  pkh3',
  runContractPure,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Prelude

{- | Contract tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "MLabsPAB.Contracts"
    [ testCase "Send ada to address" sendAda
    , testCase "Support multiple signatories" multisigSupport
    , testCase "Send native tokens" sendTokens
    , testCase "Send native tokens (without token name)" sendTokensWithoutName
    , testCase "Mint native tokens" mintTokens
    , testCase "Redeem from validator script" redeemFromValidator
    ]

sendAda :: Assertion
sendAda = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def {utxos = [(txOutRef, txOut)]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 1000)
        void $ submitTx constraints

      (result, state, _) = runContractPure contract initState

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
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def {utxos = [(txOutRef, txOut)]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy pkh3
        void $ submitTx constraints

      (result, state, _) = runContractPure contract initState
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
      txOut =
        TxOut
          (Ledger.pubKeyHashAddress pkh1)
          (Ada.lovelaceValueOf 1250 <> Value.singleton "abcd1234" "testToken" 100)
          Nothing
      initState = def {utxos = [(txOutRef, txOut)]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                pkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        void $ submitTx constraints

      (result, state, _) = runContractPure contract initState

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

sendTokensWithoutName :: Assertion
sendTokensWithoutName = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut =
        TxOut
          (Ledger.pubKeyHashAddress pkh1)
          (Ada.lovelaceValueOf 1250 <> Value.singleton "abcd1234" "" 100)
          Nothing
      initState = def {utxos = [(txOutRef, txOut)]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                pkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "" 5)
        void $ submitTx constraints

      (result, state, _) = runContractPure contract initState

  result @?= Right ()
  (state.commandHistory !! 1)
    @?= Text.replace
      "\n"
      " "
      [text|
        cardano-cli transaction build --alonzo-era
        --tx-in ${txId}#0
        --tx-in-collateral ${txId}#0
        --tx-out ${addr1}+50 + 95 abcd1234
        --tx-out ${addr2}+1000 + 5 abcd1234
        --change-address ${addr1}
        --required-signer signing-keys/signing-key-${pkh1'}.skey
        --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
      |]

mintTokens :: Assertion
mintTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def {utxos = [(txOutRef, txOut)]}
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
                  pkh2
                  (Ada.lovelaceValueOf 1000 <> Value.singleton curSymbol "testToken" 5)
        void $ submitTxConstraintsWith @Void lookups constraints

      (result, state, _) = runContractPure contract initState

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

  assertFiles
    state
    [ [text|result-scripts/policy-${curSymbol'}.plutus|]
    , [text|result-scripts/redeemer-${redeemerHash}.json|]
    , [text|signing-keys/signing-key-${pkh1'}.skey|]
    ]

redeemFromValidator :: Assertion
redeemFromValidator = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 100) Nothing
      txOutRef' = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut' = TxOut valAddr (Ada.lovelaceValueOf 1250) (Just datumHash)
      initState = def {utxos = [(txOutRef, txOut), (txOutRef', txOut')]}
      txId = encodeByteString $ fromBuiltin $ getTxId $ txOutRefId txOutRef

      validator :: Scripts.Validator
      validator =
        Scripts.mkValidatorScript
          $$(PlutusTx.compile [||(\_ _ _ -> ())||])

      valHash :: Ledger.ValidatorHash
      valHash = Ledger.validatorHash validator

      valAddr :: Ledger.Address
      valAddr = Address.scriptAddress validator

      valHash' :: Text
      valHash' =
        let (Ledger.ValidatorHash vh) = valHash
         in encodeByteString $ fromBuiltin vh

      datum :: Ledger.Datum
      datum = Ledger.Datum $ PlutusTx.toBuiltinData (11 :: Integer)

      datumHash :: Scripts.DatumHash
      datumHash = Ledger.datumHash datum

      datumHash' =
        let (Scripts.DatumHash dh) = datumHash
         in encodeByteString $ fromBuiltin dh

      redeemerHash =
        let (Scripts.RedeemerHash rh) = Ledger.redeemerHash Ledger.unitRedeemer
         in encodeByteString $ fromBuiltin rh

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        utxos <- utxosAt valAddr
        let lookups =
              Constraints.otherScript validator
                <> Constraints.otherData datum
                <> Constraints.unspentOutputs utxos
        let constraints =
              Constraints.mustSpendScriptOutput txOutRef' Ledger.unitRedeemer
                <> Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 500)
        void $ submitTxConstraintsWith @Void lookups constraints

      (result, state, _) = runContractPure contract initState

  result @?= Right ()
  (state.commandHistory !! 1)
    @?= Text.replace
      "\n"
      " "
      [text| cardano-cli transaction build --alonzo-era
       --tx-in ${txId}#1
       --tx-in-script-file result-scripts/validator-${valHash'}.plutus
       --tx-in-datum-file result-scripts/datum-${datumHash'}.json
       --tx-in-redeemer-file result-scripts/redeemer-${redeemerHash}.json
       --tx-in-collateral ${txId}#0
       --tx-out ${addr2}+500
       --change-address ${addr1}
       --required-signer signing-keys/signing-key-${pkh1'}.skey
       --mainnet --protocol-params-file ./protocol.json --out-file tx.raw
      |]
  assertFiles
    state
    [ [text|result-scripts/datum-${datumHash'}.json|]
    , [text|result-scripts/redeemer-${redeemerHash}.json|]
    , [text|result-scripts/validator-${valHash'}.plutus|]
    , [text|signing-keys/signing-key-${pkh1'}.skey|]
    ]

assertFiles :: MockContractState -> [Text] -> Assertion
assertFiles state files =
  assertBool errorMsg $
    Set.fromList (map Text.unpack files) `Set.isSubsetOf` Map.keysSet state.files
  where
    errorMsg =
      unlines
        [ "expected (at least):"
        , show files
        , "got:"
        , show (Map.keys state.files)
        ]
