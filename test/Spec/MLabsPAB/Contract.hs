{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.MLabsPAB.Contract (tests) where

import Control.Lens (ix, (&), (.~), (^.), (^?))
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (Tx, TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.TxId qualified as TxId
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (Contract (..), Endpoint, submitTx, submitTxConstraintsWith, utxosAt)
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import Spec.MockContract (
  MockContractState (..),
  addr1,
  addr2,
  commandHistory,
  files,
  pkh1,
  pkh1',
  pkh2,
  pkh3,
  pkh3',
  runContractPure,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
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
    , testCase "Multiple txs in a contract" multiTx
    ]

sendAda :: Assertion
sendAda = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text Tx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 0
        , [text|
          cardano-cli query utxo
          --address ${addr1}
          --mainnet
         |]
        )
      ,
        ( 1
        , [text|
          cardano-cli transaction calculate-min-required-utxo --alonzo-era
          --tx-out ${addr2}+1000
          --protocol-params-file ./protocol.json
          |]
        )
      ,
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 3
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file txs/tx-${outTxId}.raw
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 1
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
        |]
        )
      ,
        ( 5
        , [text|
          cardano-cli transaction sign
          --tx-body-file txs/tx-${outTxId}.raw
          --signing-key-file signing-keys/signing-key-${pkh1'}.skey
          --out-file txs/tx-${outTxId}.signed
        |]
        )
      ]

multisigSupport :: Assertion
multisigSupport = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text Tx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy pkh3
        submitTx constraints

  -- Building and siging the tx should include both signing keys
  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 3
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file txs/tx-${outTxId}.raw
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 2
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --required-signer signing-keys/signing-key-${pkh3'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 5
        , [text| 
          cardano-cli transaction sign
          --tx-body-file txs/tx-${outTxId}.raw
          --signing-key-file signing-keys/signing-key-${pkh1'}.skey
          --signing-key-file signing-keys/signing-key-${pkh3'}.skey
          --out-file txs/tx-${outTxId}.signed
          |]
        )
      ]

sendTokens :: Assertion
sendTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut =
        TxOut
          (Ledger.pubKeyHashAddress pkh1)
          (Ada.lovelaceValueOf 1250 <> Value.singleton "abcd1234" "testToken" 100)
          Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text Tx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                pkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr1}+50 + 95 abcd1234.testToken
          --tx-out ${addr2}+1000 + 5 abcd1234.testToken
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
        |]
        )
      ]

sendTokensWithoutName :: Assertion
sendTokensWithoutName = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut =
        TxOut
          (Ledger.pubKeyHashAddress pkh1)
          (Ada.lovelaceValueOf 1250 <> Value.singleton "abcd1234" "" 100)
          Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text Tx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                pkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "" 5)
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr1}+50 + 95 abcd1234
          --tx-out ${addr2}+1000 + 5 abcd1234
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
          |]
        )
      ]

mintTokens :: Assertion
mintTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

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

      contract :: Contract () (Endpoint "SendAda" ()) Text Tx
      contract = do
        let lookups =
              Constraints.mintingPolicy mintingPolicy
        let constraints =
              Constraints.mustMintValue (Value.singleton curSymbol "testToken" 5)
                <> Constraints.mustPayToPubKey
                  pkh2
                  (Ada.lovelaceValueOf 1000 <> Value.singleton curSymbol "testToken" 5)
        submitTxConstraintsWith @Void lookups constraints

  assertContractWithTxId contract initState $ \state outTxId -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text| 
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.testToken
          --mint-script-file result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file result-scripts/redeemer-${redeemerHash}.json
          --mint-execution-units (297830,1100)
          --mint 5 ${curSymbol'}.testToken
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
      |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.testToken
          --mint-script-file result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file result-scripts/redeemer-${redeemerHash}.json
          --mint 5 ${curSymbol'}.testToken
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|result-scripts/policy-${curSymbol'}.plutus|]
      , [text|result-scripts/redeemer-${redeemerHash}.json|]
      , [text|signing-keys/signing-key-${pkh1'}.skey|]
      , [text|txs/tx-${outTxId}.raw|]
      , [text|txs/tx-${outTxId}.signed|]
      ]

redeemFromValidator :: Assertion
redeemFromValidator = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 100) Nothing
      txOutRef' = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut' = TxOut valAddr (Ada.lovelaceValueOf 1250) (Just datumHash)
      initState = def & utxos .~ [(txOutRef, txOut), (txOutRef', txOut')]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

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

      contract :: Contract () (Endpoint "SendAda" ()) Text Tx
      contract = do
        utxos' <- utxosAt valAddr
        let lookups =
              Constraints.otherScript validator
                <> Constraints.otherData datum
                <> Constraints.unspentOutputs utxos'
        let constraints =
              Constraints.mustSpendScriptOutput txOutRef' Ledger.unitRedeemer
                <> Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 500)
        submitTxConstraintsWith @Void lookups constraints

  assertContractWithTxId contract initState $ \state outTxId -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#1
          --tx-in-script-file result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file result-scripts/redeemer-${redeemerHash}.json
          --tx-in-execution-units (387149,1400)
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --fee 0 --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
      |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#1
          --tx-in-script-file result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file result-scripts/redeemer-${redeemerHash}.json
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --required-signer signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file txs/tx-${outTxId}.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|result-scripts/datum-${datumHash'}.json|]
      , [text|result-scripts/redeemer-${redeemerHash}.json|]
      , [text|result-scripts/validator-${valHash'}.plutus|]
      , [text|signing-keys/signing-key-${pkh1'}.skey|]
      , [text|txs/tx-${outTxId}.raw|]
      , [text|txs/tx-${outTxId}.signed|]
      ]

multiTx :: Assertion
multiTx = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut (Ledger.pubKeyHashAddress pkh1) (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract () (Endpoint "SendAda" ()) Text [Tx]
      contract = do
        let constraints =
              Constraints.mustPayToPubKey pkh2 (Ada.lovelaceValueOf 1000)
        tx1 <- submitTx constraints
        tx2 <- submitTx constraints

        pure [tx1, tx2]

      (result, state, _) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right [tx1, tx2] ->
      let outTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txId tx1
          outTxId2 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txId tx2
       in assertFiles
            state
            [ [text|signing-keys/signing-key-${pkh1'}.skey|]
            , [text|txs/tx-${outTxId1}.raw|]
            , [text|txs/tx-${outTxId2}.raw|]
            , [text|txs/tx-${outTxId1}.signed|]
            , [text|txs/tx-${outTxId2}.signed|]
            ]
    Right _ -> assertFailure "Wrong number of txs"

assertFiles :: MockContractState -> [Text] -> Assertion
assertFiles state expectedFiles =
  assertBool errorMsg $
    Set.fromList (map Text.unpack expectedFiles) `Set.isSubsetOf` Map.keysSet (state ^. files)
  where
    errorMsg =
      unlines
        [ "expected (at least):"
        , show expectedFiles
        , "got:"
        , show (Map.keys (state ^. files))
        ]

assertContractWithTxId ::
  forall (w :: Type) (s :: Row Type).
  (Monoid w) =>
  Contract w s Text Tx ->
  MockContractState ->
  (MockContractState -> Text -> Assertion) ->
  Assertion
assertContractWithTxId contract initState assertion = do
  let (result, state, _) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right tx ->
      let outTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txId tx
       in assertion state outTxId

assertCommandHistory :: MockContractState -> [(Int, Text)] -> Assertion
assertCommandHistory state =
  mapM_
    ( \(idx, expectedCmd) ->
        (state ^? commandHistory . ix idx) @?= Just (Text.replace "\n" " " expectedCmd)
    )
