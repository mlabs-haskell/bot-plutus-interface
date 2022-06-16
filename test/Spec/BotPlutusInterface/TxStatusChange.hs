module Spec.BotPlutusInterface.TxStatusChange (tests) where

import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcOwnPubKeyHash),
 )
import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), getCardanoTxId)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (TxOut (TxOut), TxOutRef (TxOutRef))
import Plutus.ChainIndex (RollbackState (Unknown), TxStatus)
import Plutus.Contract (
  Contract (..),
  Endpoint,
  awaitTxStatusChange,
  submitTx,
 )
import Spec.MockContract (
  contractEnv,
  nonExistingTxId,
  paymentPkh1,
  paymentPkh2,
  pkhAddr1,
  runContractPure,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Await Tx status change"
    [ testCase "Return status if Tx was found as status is not Unknown" testTxFoundAndConfirmed
    , testCase "Stop waiting by timeout if Tx could not be found" testStopWaitingByTimeout
    ]

testTxFoundAndConfirmed :: Assertion
testTxFoundAndConfirmed = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1350) Nothing
      initState =
        def & utxos .~ [(txOutRef, txOut)]
          & contractEnv .~ contractEnv'
      pabConf = def {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1}
      contractEnv' = def {cePABConfig = pabConf}

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        tx <- submitTx constraints
        void $ awaitTxStatusChange $ getCardanoTxId tx

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right _, _) -> pure ()

testStopWaitingByTimeout :: Assertion
testStopWaitingByTimeout = do
  let initState =
        def & contractEnv .~ contractEnv'
      pabConf = def {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1}
      contractEnv' = def {cePABConfig = pabConf}

      contract :: Contract () (Endpoint "SendAda" ()) Text TxStatus
      contract =
        awaitTxStatusChange nonExistingTxId

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right txStatus, _) -> txStatus @?= Unknown
