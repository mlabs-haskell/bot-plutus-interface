{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.Contract (tests, commandEqual) where

import BotPlutusInterface.CardanoCLI (unsafeSerialiseAddress)
import Cardano.Api (NetworkId (Mainnet))
import Control.Lens (ix, (&), (.~), (^.), (^?))
import Data.Aeson (ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Char (isSpace)
import Data.Default (def)
import Data.Function (on)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Interval (interval)
import Ledger.Scripts qualified as Scripts
import Ledger.Slot (Slot)
import Ledger.Time (POSIXTime (POSIXTime))
import Ledger.Tx (CardanoTx, TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.TxId qualified as TxId
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.ChainIndex.Types (BlockId (..), Tip (..))
import Plutus.Contract (
  Contract (..),
  Endpoint,
  submitTx,
  submitTxConstraintsWith,
  tell,
  utxosAt,
  waitNSlots,
 )
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import Spec.MockContract (
  MockContractState (..),
  addr1,
  addr2,
  commandHistory,
  files,
  observableState,
  paymentPkh2,
  paymentPkh3,
  pkh1',
  pkh3,
  pkh3',
  pkhAddr1,
  runContractPure,
  signingKey1,
  tip,
  toSigningKeyFile,
  toVerificationKeyFile,
  utxos,
  verificationKey1,
  verificationKey3,
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
    "BotPlutusInterface.Contracts"
    [ testCase "Send ada to address" sendAda
    , testCase "Send ada to address without change" sendAdaNoChange
    , testCase "Send ada to address with staking key" sendAdaStaking
    , testCase "Support multiple signatories" multisigSupport
    , testCase "Create a tx without signing" withoutSigning
    , testCase "Send native tokens" sendTokens
    , testCase "Send native tokens (without token name)" sendTokensWithoutName
    , testCase "Mint native tokens" mintTokens
    , testCase "Spend to validator script" spendToValidator
    , testCase "Redeem from validator script" redeemFromValidator
    , testCase "Multiple txs in a contract" multiTx
    , testCase "With valid range" withValidRange
    , testCase "Use Writer in a contract" useWriter
    , testCase "Wait for next block" waitNextBlock
    ]

sendAda :: Assertion
sendAda = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1350) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  assertContract contract initState $ \state ->
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
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-?
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 1
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      , -- Steps 4 to 11 are near repeats of 1, 2 and 3, to ensure min utxo values are met, and change is dispursed

        ( 17
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --tx-out ${addr1}+50
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 300
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
        |]
        )
      ,
        ( 18
        , [text|
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-?.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --out-file ./txs/tx-?.signed
        |]
        )
      ]

sendAdaNoChange :: Assertion
sendAdaNoChange = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  assertContract contract initState $ \state ->
    assertCommandHistory
      state
      [
        ( 8
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 200
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
        |]
        )
      ]

sendAdaStaking :: Assertion
sendAdaStaking = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      stakePkh3 = Address.StakePubKeyHash pkh3
      addr2Staking = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress paymentPkh2 (Just stakePkh3))

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKeyAddress paymentPkh2 stakePkh3 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  assertContract contract initState $ \state ->
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
          --tx-out ${addr2Staking}+1000
          --protocol-params-file ./protocol.json
          |]
        )
      ,
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2Staking}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 4
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-?
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 1
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 9
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2Staking}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 200
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
        |]
        )
      ,
        ( 10
        , [text|
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-?.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --out-file ./txs/tx-?.signed
        |]
        )
      ]

multisigSupport :: Assertion
multisigSupport = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy paymentPkh3
        submitTx constraints

  -- Building and siging the tx should include both signing keys
  assertContract contract initState $ \state ->
    assertCommandHistory
      state
      [
        ( 4
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-?
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 2
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 9
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --required-signer ./signing-keys/signing-key-${pkh3'}.skey
          --fee 200
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ,
        ( 10
        , [text| 
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-?.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --signing-key-file ./signing-keys/signing-key-${pkh3'}.skey
          --out-file ./txs/tx-?.signed
          |]
        )
      ]

withoutSigning :: Assertion
withoutSigning = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState =
        def
          & utxos .~ [(txOutRef, txOut)]
          & files
            .~ Map.fromList
              [ toSigningKeyFile "./signing-keys" signingKey1
              , toVerificationKeyFile "./signing-keys" verificationKey1
              , toVerificationKeyFile "./signing-keys" verificationKey3
              ]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy paymentPkh3
        submitTx constraints

  -- Building and siging the tx should include both signing keys
  assertContract contract initState $ \state -> do
    assertCommandHistory
      state
      [
        ( 9
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --required-signer-hash ${pkh3'}
          --fee 200
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]
    assertCommandNotCalled state "cardano-cli transaction sign"

sendTokens :: Assertion
sendTokens = do
  let txOutRef1 = TxOutRef "08b27dbdcff9ab3b432638536ec7eab36c8a2e457703fb1b559dd754032ef431" 0
      txOut1 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1350 <> Value.singleton "abcd1234" "testToken" 100)
          Nothing
      txOutRef2 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut2 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1250)
          Nothing
      initState = def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)]
      inTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                paymentPkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        submitTx constraints

  assertContract contract initState $ \state ->
    assertCommandHistory
      state
      [
        ( 13
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId1}#0
          --tx-out ${addr2}+1000 + 5 abcd1234.74657374546F6B656E
          --tx-out ${addr1}+50 + 95 abcd1234.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 300
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
        |]
        )
      ]

sendTokensWithoutName :: Assertion
sendTokensWithoutName = do
  let txOutRef1 = TxOutRef "08b27dbdcff9ab3b432638536ec7eab36c8a2e457703fb1b559dd754032ef431" 0
      txOut1 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1350 <> Value.singleton "abcd1234" "" 100)
          Nothing
      txOutRef2 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut2 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1250)
          Nothing
      initState = def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)]
      inTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                paymentPkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "" 5)
        submitTx constraints

  assertContract contract initState $ \state ->
    assertCommandHistory
      state
      [
        ( 13
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId1}#0
          --tx-out ${addr2}+1000 + 5 abcd1234
          --tx-out ${addr1}+50 + 95 abcd1234
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 300
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]

mintTokens :: Assertion
mintTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1_000_000) Nothing
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

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let lookups =
              Constraints.mintingPolicy mintingPolicy
        let constraints =
              Constraints.mustMintValue (Value.singleton curSymbol "testToken" 5)
                <> Constraints.mustPayToPubKey
                  paymentPkh2
                  (Ada.lovelaceValueOf 1000 <> Value.singleton curSymbol "testToken" 5)
        submitTxConstraintsWith @Void lookups constraints

  assertContract contract initState $ \state -> do
    assertCommandHistory
      state
      [
        ( 3
        , [text| 
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.74657374546F6B656E
          --mint-script-file ./result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --mint-execution-units (500000,2000)
          --mint 5 ${curSymbol'}.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 17
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.74657374546F6B656E
          --tx-out ${addr1}+496700
          --mint-script-file ./result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --mint-execution-units (500000,2000)
          --mint 5 ${curSymbol'}.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 502300
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|./result-scripts/policy-${curSymbol'}.plutus|]
      , [text|./result-scripts/redeemer-${redeemerHash}.json|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-?.raw|]
      , [text|./txs/tx-?.signed|]
      ]

spendToValidator :: Assertion
spendToValidator = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1000) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      validator :: Scripts.Validator
      validator =
        Scripts.mkValidatorScript
          $$(PlutusTx.compile [||(\_ _ _ -> ())||])

      valHash :: Ledger.ValidatorHash
      valHash = Ledger.validatorHash validator

      valAddr :: Ledger.Address
      valAddr = Address.scriptAddress validator

      valAddr' :: Text
      valAddr' = unsafeSerialiseAddress Mainnet valAddr

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

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        utxos' <- utxosAt valAddr
        let lookups =
              Constraints.otherScript validator
                <> Constraints.otherData datum
                <> Constraints.unspentOutputs utxos'
        let constraints =
              Constraints.mustPayToOtherScript valHash datum (Ada.lovelaceValueOf 500)
        submitTxConstraintsWith @Void lookups constraints

  assertContract contract initState $ \state -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${valAddr'}+500
          --tx-out-datum-embed-file ./result-scripts/datum-${datumHash'}.json
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 17
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${valAddr'}+500
          --tx-out-datum-embed-file ./result-scripts/datum-${datumHash'}.json
          --tx-out ${addr1}+200
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 300
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|./result-scripts/datum-${datumHash'}.json|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-?.raw|]
      , [text|./txs/tx-?.signed|]
      ]

redeemFromValidator :: Assertion
redeemFromValidator = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1_000_000) Nothing
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

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        utxos' <- utxosAt valAddr
        let lookups =
              Constraints.otherScript validator
                <> Constraints.otherData datum
                <> Constraints.unspentOutputs utxos'
        let constraints =
              Constraints.mustSpendScriptOutput txOutRef' Ledger.unitRedeemer
                <> Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 500)
        submitTxConstraintsWith @Void lookups constraints

  assertContract contract initState $ \state -> do
    assertCommandHistory
      state
      [
        ( 3
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#1
          --tx-in-script-file ./result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file ./result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --tx-in-execution-units (500000,2000)
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0 --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 20
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in ${inTxId}#1
          --tx-in-script-file ./result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file ./result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --tx-in-execution-units (500000,2000)
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --tx-out ${addr1}+498350
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 502400
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|./result-scripts/datum-${datumHash'}.json|]
      , [text|./result-scripts/redeemer-${redeemerHash}.json|]
      , [text|./result-scripts/validator-${valHash'}.plutus|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-?.raw|]
      , [text|./txs/tx-?.signed|]
      ]

multiTx :: Assertion
multiTx = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract () (Endpoint "SendAda" ()) Text [CardanoTx]
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 . Ada.lovelaceValueOf
        tx1 <- submitTx $ constraints 1000
        tx2 <- submitTx $ constraints 850

        pure [tx1, tx2]

      (result, state) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right _ ->
      assertFiles
        state
        [ [text|./signing-keys/signing-key-${pkh1'}.skey|]
        , [text|./txs/tx-?.raw|]
        , [text|./txs/tx-?.raw|]
        , [text|./txs/tx-?.signed|]
        , [text|./txs/tx-?.signed|]
        ]

withValidRange :: Assertion
withValidRange = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustValidateIn (interval (POSIXTime 1643636293000) (POSIXTime 1646314693000))
        submitTx constraints

  assertContract contract initState $ \state ->
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --invalid-before 47577202
          --invalid-hereafter 50255602
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?
          |]
        )
      ,
        ( 9
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-out ${addr2}+1000
          --invalid-before 47577202
          --invalid-hereafter 50255602
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 200
          --protocol-params-file ./protocol.json --out-file ./txs/tx-?.raw
          |]
        )
      ]

useWriter :: Assertion
useWriter = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1200) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract (Last Text) (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        tell $ Last $ Just "Init contract"
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  assertContract contract initState $ \state -> do
    (state ^. observableState)
      @?= Last (Just "Init contract")

waitNextBlock :: Assertion
waitNextBlock = do
  let initSlot = 1000
      initTip = Tip initSlot (BlockId "ab12") 4
      initState = def & tip .~ initTip

      contract :: Contract () (Endpoint "SendAda" ()) Text Slot
      contract = waitNSlots 1

      (result, state) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right slot -> do
      assertBool "Current Slot is too small" (initSlot + 1 < slot)
      assertCommandHistory
        state
        [
          ( 0
          , [text| cardano-cli query tip --mainnet |]
          )
        ]

assertFiles :: forall (w :: Type). MockContractState w -> [Text] -> Assertion
assertFiles state expectedFiles =
  assertBool errorMsg $
    List.null $ List.deleteFirstsBy (flip commandEqual) expectedFiles fileNames
  where
    fileNames = Text.pack <$> Map.keys (state ^. files)
    errorMsg =
      unlines
        [ "expected (at least):"
        , show expectedFiles
        , "got:"
        , show (Map.keys (state ^. files))
        ]

assertContractWithTxId ::
  forall (w :: Type) (s :: Row Type).
  (ToJSON w, Monoid w) =>
  Contract w s Text CardanoTx ->
  MockContractState w ->
  (MockContractState w -> Text -> Assertion) ->
  Assertion
assertContractWithTxId contract initState assertion = do
  let (result, state) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right tx ->
      let outTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.getCardanoTxId tx
       in assertion state outTxId

assertContract ::
  forall (w :: Type) (s :: Row Type).
  (ToJSON w, Monoid w) =>
  Contract w s Text CardanoTx ->
  MockContractState w ->
  (MockContractState w -> Assertion) ->
  Assertion
assertContract contract initState assertion = do
  let (result, state) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right _ -> assertion state

assertCommandHistory :: forall (w :: Type). MockContractState w -> [(Int, Text)] -> Assertion
assertCommandHistory state =
  mapM_
    ( \(idx, expectedCmd) ->
        assertCommandEqual
          ("command at index " ++ show idx ++ " was incorrect:")
          expectedCmd
          (fromMaybe "" $ state ^? commandHistory . ix idx)
    )

-- | assertEqual but using `commandEqual`
assertCommandEqual :: String -> Text -> Text -> Assertion
assertCommandEqual err expected actual
  | commandEqual expected actual = return ()
  | otherwise = assertFailure $ err ++ "\nExpected:\n" ++ show expected ++ "\nGot:\n" ++ show actual

{- | Checks if a command matches an expected command pattern
 Where a command pattern may use new lines in place of spaces, and use the wildcard `?` to match up to the next space
 E.g. `commandEqual "123\n456 ? 0" "123 456 789 0"` == `True`
-}
commandEqual :: Text -> Text -> Bool
commandEqual "" "" = True
commandEqual "" _ = False
commandEqual _ "" = False
commandEqual expected actual = maybe False (on commandEqual dropToSpace postExp) mPostAct
  where
    (preExp, postExp) = Text.breakOn "?" expected
    mPostAct = Text.stripPrefix (Text.replace "\n" " " preExp) actual
    dropToSpace = Text.dropWhile (not . isSpace)

assertCommandCalled :: forall (w :: Type). MockContractState w -> Text -> Assertion
assertCommandCalled state expectedCmd =
  assertBool
    (Text.unpack . Text.unlines $ ["Command was not called:", expectedCmd])
    (any (Text.isInfixOf (Text.replace "\n" " " expectedCmd)) (state ^. commandHistory))

assertCommandNotCalled :: forall (w :: Type). MockContractState w -> Text -> Assertion
assertCommandNotCalled state expectedCmd =
  assertBool
    (Text.unpack . Text.unlines $ ["Command was called:", expectedCmd])
    (not (any (Text.isInfixOf (Text.replace "\n" " " expectedCmd)) (state ^. commandHistory)))
