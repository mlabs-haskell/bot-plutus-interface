module Spec.BotPlutusInterface.TxStatusChange (tests) where

import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcOwnPubKeyHash, pcTxStatusPolling),
  TxStatusPolling (spBlocksTimeOut),
 )
import Control.Lens ((&), (.~), (^.))
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), getCardanoTxId)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (TxOut (TxOut), TxOutRef (TxOutRef))
import Plutus.ChainIndex (RollbackState (Unknown), Tip (TipAtGenesis), TxStatus)
import Plutus.ChainIndex.Types (Tip (Tip))
import Plutus.Contract (
  Contract (..),
  Endpoint,
  awaitTxStatusChange,
  getTip,
  submitTx,
  throwError,
 )
import Spec.MockContract (
  contractEnv,
  nonExistingTxId,
  paymentPkh1,
  paymentPkh2,
  pkhAddr1,
  runContractPure,
  tip,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
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

      contract :: Contract () (Endpoint "SendAda" ()) Text TxStatus
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        tx <- submitTx constraints
        awaitTxStatusChange $ getCardanoTxId tx

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right Unknown, _) -> assertFailure "Tx status should not be Unknown but it is"
    (Right _, _) -> pure ()

testStopWaitingByTimeout :: Assertion
testStopWaitingByTimeout = do
  let initState =
        def & contractEnv .~ contractEnv'
      pabConf = def {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1}
      timeoutBlocks = fromIntegral . spBlocksTimeOut . pcTxStatusPolling $ pabConf
      contractEnv' = def {cePABConfig = pabConf}

      contract :: Contract () (Endpoint "SendAda" ()) Text (Tip, TxStatus)
      contract = do
        awaitStartBlock <- getTip
        case awaitStartBlock of
          TipAtGenesis -> throwError "Should not happen: TipAtGenesis"
          tip' -> do
            txStatus <- awaitTxStatusChange nonExistingTxId
            pure (tip', txStatus)

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right (startTip, txStatus), state) -> do
      startAwaitingBlockNo <- getBlock startTip
      endAwaitingBlockNo <- getBlock $ state ^. tip
      assertBool
        "Current block should be GT than start + timeout"
        (endAwaitingBlockNo > startAwaitingBlockNo + timeoutBlocks)
      txStatus @?= Unknown
  where
    getBlock = \case
      TipAtGenesis -> assertFailure "Should not happen: TipAtGenesis"
      Tip _ _ blockNo -> pure blockNo
