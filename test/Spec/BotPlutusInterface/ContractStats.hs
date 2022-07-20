module Spec.BotPlutusInterface.ContractStats (tests) where

import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcCollectStats, pcOwnPubKeyHash),
 )
import Control.Lens ((&), (.~), (^.))
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (ChainIndexTxOut (PublicKeyChainIndexTxOut), PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (CardanoTx, TxOutRef (TxOutRef))
import Plutus.Contract (
  Contract (..),
  Endpoint,
  submitTx,
 )
import Spec.MockContract (
  contractEnv,
  mockBudget,
  paymentPkh1,
  paymentPkh2,
  pkhAddr1,
  runContractPure,
  statsUpdates,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Collecting contract stats"
    [ testCase "Budget added when saving enabled" budgetSavingEnabled
    , testCase "Budget NOT added with default (`False`) option" budgetSavingDisabled
    ]

budgetSavingEnabled :: Assertion
budgetSavingEnabled = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 1350)
      initState =
        def & utxos .~ [(txOutRef, txOut)]
          & contractEnv .~ contractEnv'
      pabConf = def {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1, pcCollectStats = True}
      contractEnv' = def {cePABConfig = pabConf}

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right _, state) -> state ^. statsUpdates @?= [mockBudget]

budgetSavingDisabled :: Assertion
budgetSavingDisabled = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 1350)
      initState = def & utxos .~ [(txOutRef, txOut)]

      contract :: Contract () (Endpoint "SendAda" ()) Text CardanoTx
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        submitTx constraints

  case runContractPure contract initState of
    (Left err, _) -> assertFailure $ Text.unpack err
    (Right _, state) -> state ^. statsUpdates @?= mempty
