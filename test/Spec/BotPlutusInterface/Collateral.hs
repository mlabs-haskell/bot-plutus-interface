module Spec.BotPlutusInterface.Collateral where

import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcOwnPubKeyHash), CollateralUtxo (CollateralUtxo)
 )
import Cardano.Api (TxBodyContent (txIns))
import Cardano.Api qualified as C
import Control.Lens ((&), (.~))
import Control.Monad (when)
import Data.Default (def)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), Tx (txCollateral, txInputs), txInRef)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (CardanoTx, TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx.CardanoAPI qualified as C
import Plutus.Contract (
  Contract (..),
  Endpoint,
  submitTx,
  throwError,
 )
import Spec.MockContract (
  contractEnv,
  paymentPkh1,
  paymentPkh2,
  pkhAddr1,
  runContractPure,
  theCollateralUtxo,
  utxos,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Doesn't spend collateral."
    [ testCase "Use collateral utxo as collateral and not as input" testTxUsesCollateralCorrectly
    ]

-- | check that tx doesn't spend collateral but uses it as collateral
testTxUsesCollateralCorrectly :: Assertion
testTxUsesCollateralCorrectly = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1350) Nothing
      collateralTxOut = TxOut pkhAddr1 (Ada.lovelaceValueOf 1350) Nothing
      (CollateralUtxo collateralTxOutRef) = theCollateralUtxo

      -- lets test both orders, strzeżonego pan Bóg strzeże
      utxos1 = [(collateralTxOutRef, collateralTxOut), (txOutRef, txOut)]
      utxos2 = [(txOutRef, txOut), (collateralTxOutRef, collateralTxOut)]
      initState _utxos =
        def & utxos .~ _utxos
          & contractEnv .~ contractEnv'
      pabConf = def {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1}
      contractEnv' = def {cePABConfig = pabConf}

      contract :: Contract () (Endpoint "SendAda" ()) Text ()
      contract = do
        let constraints =
              Constraints.mustPayToPubKey paymentPkh2 (Ada.lovelaceValueOf 1000)
        tx <- submitTx constraints

        let collateralInInputs = Set.member collateralTxOutRef $ getCardanoTxInputs tx
            expectedCollaterals = Just (Set.fromList [collateralTxOutRef])
            collateralsAreUnexpected = expectedCollaterals /= getCardanoTxCollateralInputs tx

        -- check that tx doesn't use the collateral in inputs and does in collaterals
        when collateralInInputs $ throwError "Tx spends utxo chosen for collateral."
        when collateralsAreUnexpected $
          throwError @Text
            ( "Unexpected collaterals. Expected: " <> pack (show expectedCollaterals)
                <> ". Actual: "
                <> pack (show (getCardanoTxCollateralInputs tx))
            )

  -- run the same contract with two orders of starting utxos
  let (res1, _) = runContractPure contract (initState utxos1)
      (res2, _) = runContractPure contract (initState utxos2)

  -- if both fail show only one error message
  case res1 >> res2 of
    Left err -> assertFailure $ Text.unpack err
    Right _ -> pure ()

getCardanoTxInputs :: CardanoTx -> Set.Set TxOutRef
getCardanoTxInputs = \case
  (Left (C.SomeTx (C.Tx (C.TxBody C.TxBodyContent {txIns = _txIns}) _) _)) ->
    Set.fromList $ fmap (C.fromCardanoTxIn . fst) _txIns
  (Right tx) -> Set.map txInRef $ txInputs tx

getCardanoTxCollateralInputs :: CardanoTx -> Maybe (Set.Set TxOutRef)
getCardanoTxCollateralInputs = \case
  (Left (C.SomeTx (C.Tx (C.TxBody C.TxBodyContent {txInsCollateral = C.TxInsCollateral _ _txIns}) _) _)) ->
    Just $ Set.fromList $ fmap C.fromCardanoTxIn _txIns
  (Left (C.SomeTx (C.Tx (C.TxBody C.TxBodyContent {txInsCollateral = C.TxInsCollateralNone}) _) _)) ->
    Nothing
  (Right tx) -> Just $ Set.map txInRef $ txCollateral tx
