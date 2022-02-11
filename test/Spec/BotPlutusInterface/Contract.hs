{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.Contract (tests) where

import BotPlutusInterface.CardanoCLI (unsafeSerialiseAddress)
import Cardano.Api (NetworkId (Mainnet))
import Control.Lens (ix, (&), (.~), (^.), (^?))
import Data.Aeson (ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Set qualified as Set
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
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
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
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 3
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-${outTxId}.raw
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 1
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
        |]
        )
      ,
        ( 7
        , [text|
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-${outTxId}.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --out-file ./txs/tx-${outTxId}.signed
        |]
        )
      ]

sendAdaStaking :: Assertion
sendAdaStaking = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      stakePkh3 = Address.StakePubKeyHash pkh3
      addr2Staking = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress paymentPkh2 (Just stakePkh3))

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKeyAddress paymentPkh2 stakePkh3 (Ada.lovelaceValueOf 1000)
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
          --tx-out ${addr2Staking}+1000
          --protocol-params-file ./protocol.json
          |]
        )
      ,
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2Staking}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 3
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-${outTxId}.raw
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 1
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2Staking}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
        |]
        )
      ,
        ( 7
        , [text|
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-${outTxId}.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --out-file ./txs/tx-${outTxId}.signed
        |]
        )
      ]

multisigSupport :: Assertion
multisigSupport = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract Text (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustBeSignedBy paymentPkh3
        submitTx constraints

  -- Building and siging the tx should include both signing keys
  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 3
        , [text|
          cardano-cli transaction calculate-min-fee
          --tx-body-file ./txs/tx-${outTxId}.raw
          --tx-in-count 1
          --tx-out-count 1
          --witness-count 2
          --protocol-params-file ./protocol.json
          --mainnet
          |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --required-signer ./signing-keys/signing-key-${pkh3'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 7
        , [text| 
          cardano-cli transaction sign
          --tx-body-file ./txs/tx-${outTxId}.raw
          --signing-key-file ./signing-keys/signing-key-${pkh1'}.skey
          --signing-key-file ./signing-keys/signing-key-${pkh3'}.skey
          --out-file ./txs/tx-${outTxId}.signed
          |]
        )
      ]

withoutSigning :: Assertion
withoutSigning = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
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
  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --required-signer-hash ${pkh3'}
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ]

sendTokens :: Assertion
sendTokens = do
  let txOutRef1 = TxOutRef "08b27dbdcff9ab3b432638536ec7eab36c8a2e457703fb1b559dd754032ef431" 0
      txOut1 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1300 <> Value.singleton "abcd1234" "testToken" 100)
          Nothing
      txOutRef2 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut2 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1250)
          Nothing
      initState = def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)]
      inTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1
      inTxId2 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef2

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                paymentPkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "testToken" 5)
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 10
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId1}#0
          --tx-in-collateral ${inTxId2}#1
          --tx-out ${addr1}+50 + 95 abcd1234.74657374546F6B656E
          --tx-out ${addr2}+1000 + 5 abcd1234.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
        |]
        )
      ]

sendTokensWithoutName :: Assertion
sendTokensWithoutName = do
  let txOutRef1 = TxOutRef "08b27dbdcff9ab3b432638536ec7eab36c8a2e457703fb1b559dd754032ef431" 0
      txOut1 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1300 <> Value.singleton "abcd1234" "" 100)
          Nothing
      txOutRef2 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 1
      txOut2 =
        TxOut
          pkhAddr1
          (Ada.lovelaceValueOf 1250)
          Nothing
      initState = def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)]
      inTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1
      inTxId2 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef2

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey
                paymentPkh2
                (Ada.lovelaceValueOf 1000 <> Value.singleton "abcd1234" "" 5)
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 10
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId1}#0
          --tx-in-collateral ${inTxId2}#1
          --tx-out ${addr1}+50 + 95 abcd1234
          --tx-out ${addr2}+1000 + 5 abcd1234
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ]

mintTokens :: Assertion
mintTokens = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
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

  assertContractWithTxId contract initState $ \state outTxId -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text| 
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.74657374546F6B656E
          --mint-script-file ./result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --mint-execution-units (297830,1100)
          --mint 5 ${curSymbol'}.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
      |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000 + 5 ${curSymbol'}.74657374546F6B656E
          --mint-script-file ./result-scripts/policy-${curSymbol'}.plutus
          --mint-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --mint 5 ${curSymbol'}.74657374546F6B656E
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|./result-scripts/policy-${curSymbol'}.plutus|]
      , [text|./result-scripts/redeemer-${redeemerHash}.json|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-${outTxId}.raw|]
      , [text|./txs/tx-${outTxId}.signed|]
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

  assertContractWithTxId contract initState $ \state outTxId -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${valAddr'}+500
          --tx-out-datum-embed-file ./result-scripts/datum-${datumHash'}.json
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0 --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
      |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${valAddr'}+500
          --tx-out-datum-embed-file ./result-scripts/datum-${datumHash'}.json
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ]
    --tx-out-datum-file result-scripts/datum-${datumHash'}.json

    assertFiles
      state
      [ [text|./result-scripts/datum-${datumHash'}.json|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-${outTxId}.raw|]
      , [text|./txs/tx-${outTxId}.signed|]
      ]

redeemFromValidator :: Assertion
redeemFromValidator = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 100) Nothing
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

  assertContractWithTxId contract initState $ \state outTxId -> do
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#1
          --tx-in-script-file ./result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file ./result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --tx-in-execution-units (387149,1400)
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0 --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
      |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#1
          --tx-in-script-file ./result-scripts/validator-${valHash'}.plutus
          --tx-in-datum-file ./result-scripts/datum-${datumHash'}.json
          --tx-in-redeemer-file ./result-scripts/redeemer-${redeemerHash}.json
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+500
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ]

    assertFiles
      state
      [ [text|./result-scripts/datum-${datumHash'}.json|]
      , [text|./result-scripts/redeemer-${redeemerHash}.json|]
      , [text|./result-scripts/validator-${valHash'}.plutus|]
      , [text|./signing-keys/signing-key-${pkh1'}.skey|]
      , [text|./txs/tx-${outTxId}.raw|]
      , [text|./txs/tx-${outTxId}.signed|]
      ]

multiTx :: Assertion
multiTx = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract () (Endpoint "SendAda" ()) Text [CardanoTx]
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        tx1 <- submitTx constraints
        tx2 <- submitTx constraints

        pure [tx1, tx2]

      (result, state) = runContractPure contract initState

  case result of
    Left errMsg -> assertFailure (show errMsg)
    Right [tx1, tx2] ->
      let outTxId1 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.getCardanoTxId tx1
          outTxId2 = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.getCardanoTxId tx2
       in assertFiles
            state
            [ [text|./signing-keys/signing-key-${pkh1'}.skey|]
            , [text|./txs/tx-${outTxId1}.raw|]
            , [text|./txs/tx-${outTxId2}.raw|]
            , [text|./txs/tx-${outTxId1}.signed|]
            , [text|./txs/tx-${outTxId2}.signed|]
            ]
    Right _ -> assertFailure "Wrong number of txs"

withValidRange :: Assertion
withValidRange = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
                <> Constraints.mustValidateIn (interval (POSIXTime 1643636293000) (POSIXTime 1646314693000))
        submitTx constraints

  assertContractWithTxId contract initState $ \state outTxId ->
    assertCommandHistory
      state
      [
        ( 2
        , [text|
          cardano-cli transaction build-raw --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --invalid-before 47577202
          --invalid-hereafter 50255602
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --fee 0
          --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
          |]
        )
      ,
        ( 6
        , [text|
          cardano-cli transaction build --alonzo-era
          --tx-in ${inTxId}#0
          --tx-in-collateral ${inTxId}#0
          --tx-out ${addr2}+1000
          --invalid-before 47577202
          --invalid-hereafter 50255602
          --required-signer ./signing-keys/signing-key-${pkh1'}.skey
          --change-address ${addr1}
          --mainnet --protocol-params-file ./protocol.json --out-file ./txs/tx-${outTxId}.raw
        |]
        )
      ]

useWriter :: Assertion
useWriter = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1250) Nothing
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract (Last Text) (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        tell $ Last $ Just "Init contract"
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        txId <- submitTx constraints
        tell $ Last $ Just $ Text.pack $ show $ Tx.txId <$> txId
        pure txId

  assertContractWithTxId contract initState $ \state outTxId -> do
    (state ^. observableState)
      @?= Last (Just ("Right " <> outTxId))

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

assertCommandHistory :: forall (w :: Type). MockContractState w -> [(Int, Text)] -> Assertion
assertCommandHistory state =
  mapM_
    ( \(idx, expectedCmd) ->
        (state ^? commandHistory . ix idx) @?= Just (Text.replace "\n" " " expectedCmd)
    )
