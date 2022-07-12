{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.Collateral where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Text (Text, pack)
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (CardanoTx, TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.TxId qualified as TxId
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (
  Contract (..),
  Endpoint,
  submitTxConstraintsWith,
 )
import Spec.MockContract (
  addr1,
  addr2,
  collateralUtxo,
  contractEnv,
  paymentPkh2,
  pkh1',
  pkhAddr1,
  runContractPure,
  utxos,
 )

import BotPlutusInterface.Types (CollateralUtxo (CollateralUtxo), CollateralVar (CollateralVar), ContractEnvironment (..), PABConfig (pcCollateralSize))
import Control.Concurrent.STM (newTVarIO)

import Spec.BotPlutusInterface.Contract (assertCommandHistory, assertContract)

import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Doesn't spend collateral."
    [ testCase
        "Use collateral utxo present in the user's wallet, instead of creating new one."
        testTxUsesCollateralCorrectly
    , testCase "create collateral utxo" testTxCreatesCollateralCorrectly
    ]

-- Test to check that correct UTxo is selected from user's wallet as collateral.
testTxUsesCollateralCorrectly :: Assertion
testTxUsesCollateralCorrectly = do
  let txOutRef1 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut1 = TxOut pkhAddr1 (Ada.lovelaceValueOf 10_000_000) Nothing
      txOutRef2 = TxOutRef "d406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e4" 0
      txOut2 = TxOut pkhAddr1 (Ada.lovelaceValueOf 90_000_000) Nothing
      cenv' = def {ceCollateral = CollateralVar $ unsafePerformIO $ newTVarIO Nothing}
      initState = def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)] & contractEnv .~ cenv' & collateralUtxo .~ Nothing

      collatUtxo = Just $ CollateralUtxo txOutRef1

      collateralTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1
      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef2

  assertContract mintContract initState $ \state -> do
    assertEqual
      ("InValid collateral. Expected: " <> show collatUtxo <> " but Got: " <> show (state ^. collateralUtxo))
      collatUtxo
      (state ^. collateralUtxo)

    assertCommandHistory
      state
      [
        ( 3
        , [text|
            cardano-cli transaction build-raw --alonzo-era
            --tx-in ${inTxId}#0
            --tx-in-collateral ${collateralTxId}#0
            --tx-out ${addr2}+1000 + 5 648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198.74657374546F6B656E
            --mint-script-file ./result-scripts/policy-648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198.plutus
            --mint-redeemer-file ./result-scripts/redeemer-923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec.json
            --mint-execution-units (0,0)
            --mint 5 648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198.74657374546F6B656E
            --required-signer ./signing-keys/signing-key-${pkh1'}.skey
            --fee 0 --protocol-params-file ./protocol.json
            --out-file ./txs/tx-9e13584e45ce4c310f2b0f14341b9ab51bd3ec7978caeaf8e395f7a54315f94e.raw
          |]
        )
      ]

-- Test to check that collateral UTxo is first created if it is not present in the user's wallet.
testTxCreatesCollateralCorrectly :: Assertion
testTxCreatesCollateralCorrectly = do
  let txOutRef1 = TxOutRef "d406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e4" 0
      txOut1 = TxOut pkhAddr1 (Ada.lovelaceValueOf 90_000_000) Nothing
      cenv' = def {ceCollateral = CollateralVar $ unsafePerformIO $ newTVarIO Nothing}
      initState = def & utxos .~ [(txOutRef1, txOut1)] & contractEnv .~ cenv' & collateralUtxo .~ Nothing

      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1

      (_, state) = runContractPure mintContract initState
      collatVal = pack $ show $ pcCollateralSize $ cePABConfig (state ^. contractEnv)

  assertCommandHistory
    state
    [
      ( 2
      , [text|
         cardano-cli transaction calculate-min-required-utxo
         --alonzo-era
         --tx-out ${addr1}+${collatVal}
         --protocol-params-file ./protocol.json
       |]
      )
    ,
      ( 3
      , [text|
         cardano-cli transaction build-raw
         --alonzo-era
         --tx-in ${inTxId}#0
         --tx-out ${addr1}+${collatVal}
         --required-signer ./signing-keys/signing-key-${pkh1'}.skey
         --fee 0
         --protocol-params-file ./protocol.json
         --out-file ./txs/tx-fa4c303a2d6feb62b43440d6e0b9a90f5b20b00ecfe5364b6927806b0e8e0198.raw
       |]
      )
    ]

curSymbol :: Value.CurrencySymbol
curSymbol = Ledger.scriptCurrencySymbol mintingPolicy

curSymbol' :: Text
curSymbol' = encodeByteString $ fromBuiltin $ Value.unCurrencySymbol curSymbol

mintContract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
mintContract = do
  let lookups =
        Constraints.mintingPolicy mintingPolicy
  let constraints =
        Constraints.mustMintValue (Value.singleton curSymbol "testToken" 5)
          <> Constraints.mustPayToPubKey
            paymentPkh2
            (Ada.lovelaceValueOf 1000 <> Value.singleton curSymbol "testToken" 5)
  submitTxConstraintsWith @Void lookups constraints

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||(\_ _ -> ())||])
