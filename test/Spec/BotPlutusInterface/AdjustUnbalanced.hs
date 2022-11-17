module Spec.BotPlutusInterface.AdjustUnbalanced (tests) where

import BotPlutusInterface.Types (
  ContractEnvironment (cePABConfig),
  PABConfig (pcNetwork, pcOwnPubKeyHash, pcProtocolParams),
 )
import Cardano.Prelude (note)
import Control.Lens ((%~), (&), (.~), (^.))
import Data.Default (def)
import Data.Text (Text)
import Ledger (
  ChainIndexTxOut (PublicKeyChainIndexTxOut),
  Params (Params),
  PaymentPubKeyHash (unPaymentPubKeyHash),
  TxOut (..),
  Value,
  outputs,
  pubKeyHashAddress,
  txOutAddress,
  txOutValue,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (TxOutRef (TxOutRef))
import Plutus.Contract (
  Contract (..),
  Endpoint,
  adjustUnbalancedTx,
  throwError,
 )
import Spec.MockContract (
  contractEnv,
  paymentPkh1,
  paymentPkh2,
  paymentPkh3,
  pkhAddr1,
  runContractPure,
  updatePabConfig,
  utxos,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Prelude

import Data.Foldable (find)
import Data.Void (Void)
import Ledger.Ada (fromValue)
import Ledger.Constraints.OffChain (tx)

tests :: TestTree
tests = testCase "Adjusting unbalanced transaction" testOutsGetAdjusted

testOutsGetAdjusted :: Assertion
testOutsGetAdjusted = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 1350) Nothing Nothing
      initState =
        def & utxos .~ [(txOutRef, txOut)]
          & contractEnv
            %~ updatePabConfig
              (\pc -> pc {pcOwnPubKeyHash = unPaymentPubKeyHash paymentPkh1})

      smallValue = Ada.lovelaceValueOf 1
      bigEnoughValue = Ada.adaValueOf 777

      shouldBeAdjusted = (paymentPkh2, smallValue)
      shouldNotBeAdjusted = (paymentPkh3, bigEnoughValue)

      pabConf = cePABConfig $ initState ^. contractEnv
      contract :: Contract () (Endpoint "SendAda" ()) Text [TxOut]
      contract = do
        pparams <- note "Must have ProtocolParams set in PABConfig" $ pcProtocolParams pabConf

        let constraints = foldMap toPayConstraint [shouldBeAdjusted, shouldNotBeAdjusted]
            utx =
              either
                (error . show)
                id
                ( Constraints.mkTxWithParams @Void
                    (Params def pparams (pcNetwork pabConf))
                    mempty
                    constraints
                )
        adjustedUtx <- adjustUnbalancedTx utx
        return (adjustedUtx ^. tx . outputs)

  case runContractPure contract initState of
    (Right outs, _) -> do
      -- check of value that should be adjusted
      assertBool
        "Small values should be adjusted and become bigger"
        (fromValue (outValueForPkh outs paymentPkh2) > fromValue smallValue)

      -- check of value that should NOT be adjusted
      let resultAdaAmount = fromValue (outValueForPkh outs paymentPkh3)
          initialAdaAmount = fromValue bigEnoughValue
          errMessage =
            "Big enough value should not be adjusted, but it changed: "
              <> show initialAdaAmount
              <> " -> "
              <> show resultAdaAmount
      assertBool errMessage (resultAdaAmount == initialAdaAmount)
    e -> assertFailure $ "RES:\n" ++ show e

toPayConstraint :: (PaymentPubKeyHash, Value) -> Constraints.TxConstraints i o
toPayConstraint (pkh, value) = Constraints.mustPayToPubKey pkh value

outValueForPkh :: [TxOut] -> PaymentPubKeyHash -> Value
outValueForPkh outs pkh =
  let address = pubKeyHashAddress pkh Nothing
   in maybe
        (error "Should not happen: value for PKH used in test not found")
        txOutValue
        $ flip find outs $ \txOut -> address == txOutAddress txOut
