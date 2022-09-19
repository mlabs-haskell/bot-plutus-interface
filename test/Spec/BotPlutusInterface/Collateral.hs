{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BotPlutusInterface.Collateral where

import Control.Lens ((&), (.~), (<>~), (^.))
import Data.Aeson.Extras (encodeByteString)
import Data.Default (def)
import Data.Text (Text, pack)
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (CardanoTx, ChainIndexTxOut (PublicKeyChainIndexTxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.Tx qualified as TxId
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.Contract (
  Contract,
  EmptySchema,
  Endpoint,
  ownAddresses,
  submitTxConstraintsWith,
  txOutFromRef,
  unspentTxOutFromRef,
  utxoRefMembership,
  utxoRefsAt,
  utxoRefsWithCurrency,
  utxosAt,
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
  theCollateralUtxo,
  utxos,
 )

import BotPlutusInterface.Types (
  CollateralUtxo (CollateralUtxo, collateralTxOutRef),
  CollateralVar (CollateralVar),
  ContractEnvironment (ceCollateral, cePABConfig),
  PABConfig (pcCollateralSize),
 )
import Control.Concurrent.STM (newTVarIO)

import Spec.BotPlutusInterface.Contract (assertCommandHistory, assertContract)

import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), assetClass)
import Plutus.ChainIndex (Page (pageItems), PageQuery (PageQuery), PageSize (PageSize))
import Plutus.ChainIndex.Api (IsUtxoResponse, UtxosResponse, isUtxo, page)
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Collateral handling"
    [ testCase
        "Should use collateral utxo present in the user's wallet, instead of creating new one."
        testTxUsesCollateralCorrectly
    , testCase
        "Should create collateral utxo if not present in user's wallets"
        testTxCreatesCollateralCorrectly
    , testCase
        "Should not return collateral utxo in chain-index responses"
        testCollateralFiltering
    ]

-- Test to check that correct UTxo is selected from user's wallet as collateral.
testTxUsesCollateralCorrectly :: Assertion
testTxUsesCollateralCorrectly = do
  let -- txOutRef1 should be picked up and used as collateral UTxO
      txOutRef1 = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut1 = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 10_000_000) Nothing Nothing
      txOutRef2 = TxOutRef "d406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e4" 0
      txOut2 = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 90_000_000) Nothing Nothing
      cenv' = def {ceCollateral = CollateralVar $ unsafePerformIO $ newTVarIO Nothing}
      initState =
        def & utxos .~ [(txOutRef1, txOut1), (txOutRef2, txOut2)]
          & contractEnv .~ cenv'
          & collateralUtxo .~ Nothing

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
            cardano-cli transaction build-raw --babbage-era
            --tx-in ${inTxId}#0
            --tx-in-collateral ${collateralTxId}#0
            --tx-out ${addr2}+3000000 + 5 363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f.74657374546F6B656E
            --mint-script-file ./result-scripts/policy-363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f.plutus
            --mint-redeemer-file ./result-scripts/redeemer-923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec.json
            --mint-execution-units (0,0)
            --mint 5 363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f.74657374546F6B656E
            --required-signer ./signing-keys/signing-key-${pkh1'}.skey
            --fee 0 --protocol-params-file ./protocol.json
            --out-file ./txs/tx-?.raw
          |]
        )
      ]

-- Test to check that collateral UTxo is first created if it is not present in the user's wallet.
testTxCreatesCollateralCorrectly :: Assertion
testTxCreatesCollateralCorrectly = do
  let txOutRef1 = TxOutRef "d406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e4" 0
      txOut1 = PublicKeyChainIndexTxOut pkhAddr1 (Ada.lovelaceValueOf 90_000_000) Nothing Nothing
      cenv' = def {ceCollateral = CollateralVar $ unsafePerformIO $ newTVarIO Nothing}
      initState = def & utxos .~ [(txOutRef1, txOut1)] & contractEnv .~ cenv' & collateralUtxo .~ Nothing

      inTxId = encodeByteString $ fromBuiltin $ TxId.getTxId $ Tx.txOutRefId txOutRef1

      (_, state) = runContractPure mintContract initState
      collatVal = pack $ show $ pcCollateralSize $ cePABConfig (state ^. contractEnv)

  assertCommandHistory
    state
    [
      ( 3
      , [text|
         cardano-cli transaction build-raw
         --babbage-era
         --tx-in ${inTxId}#0
         --tx-out ${addr1}+${collatVal}
         --required-signer ./signing-keys/signing-key-${pkh1'}.skey
         --fee 0
         --protocol-params-file ./protocol.json
         --out-file ./txs/tx-?.raw
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
        Constraints.plutusV1MintingPolicy mintingPolicy
  let constraints =
        Constraints.mustMintValue (Value.singleton curSymbol "testToken" 5)
          <> Constraints.mustPayToPubKey
            paymentPkh2
            ( Ada.adaValueOf 3 -- use big enough Value to not to deal with min Ada adjustment
                <> Value.singleton curSymbol "testToken" 5
            )
  submitTxConstraintsWith @Void lookups constraints

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||(\_ _ -> ())||])

type ContractResult =
  ( Maybe TxId.ChainIndexTxOut
  , Maybe TxId.ChainIndexTxOut
  , IsUtxoResponse
  , UtxosResponse
  , UtxosResponse
  , Map TxOutRef TxId.ChainIndexTxOut
  )
testCollateralFiltering :: Assertion
testCollateralFiltering = do
  let txOutRef = TxOutRef "e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" 0
      txOut = PublicKeyChainIndexTxOut pkhAddr1 (Ada.adaValueOf 50) Nothing Nothing
      initState = def & utxos <>~ [(txOutRef, txOut)]

      collateralOref = collateralTxOutRef theCollateralUtxo

      contract :: Plutus.Contract.Contract () EmptySchema Text ContractResult
      contract = do
        (ownAddress :| _) <- ownAddresses
        txOutFromRef' <- txOutFromRef collateralOref
        unspentTxOutFromRef' <- unspentTxOutFromRef collateralOref
        utxoRefMembership' <- utxoRefMembership collateralOref
        utxoRefsAt' <- utxoRefsAt (PageQuery (PageSize 10) Nothing) ownAddress

        let adaAsset = assetClass (CurrencySymbol "") (TokenName "")
        utxoRefsWithCurrency' <- utxoRefsWithCurrency (PageQuery (PageSize 10) Nothing) adaAsset
        utxosAt' <- utxosAt ownAddress
        pure
          ( txOutFromRef'
          , unspentTxOutFromRef'
          , utxoRefMembership'
          , utxoRefsAt'
          , utxoRefsWithCurrency'
          , utxosAt'
          )

  case runContractPure contract initState of
    ( Right
        ( txOutFromRef'
          , unspentTxOutFromRef'
          , utxoRefMembership'
          , utxoRefsAt'
          , utxoRefsWithCurrency'
          , utxosAt'
          )
      , st
      ) -> do
        assertCollateralInDistribution collateralOref (st ^. utxos)
        assertCollateralNotRetunredBy
          "txOutFromRef"
          txOutFromRef'
        assertCollateralNotRetunredBy
          "unspentTxOutFromRef"
          unspentTxOutFromRef'
        assertBool
          "collateral should not be member of UTxO set"
          (not $ isUtxo utxoRefMembership')
        assertNotFoundIn
          "utxoRefsAt response"
          collateralOref
          (pageItems $ page utxoRefsAt')
        assertNotFoundIn
          "utxoRefsWithCurrency response"
          collateralOref
          (pageItems $ page utxoRefsWithCurrency')
        assertNotFoundIn
          "utxosAt response"
          collateralOref
          (Map.keys utxosAt')
    (Left e, _) -> assertFailure $ "Contract execution failed: " <> show e
  where
    assertCollateralNotRetunredBy request txOutFromRef' =
      assertBool (request <> " should return Nothing for collateral UTxO") (isNothing txOutFromRef')

    assertNotFoundIn :: (Foldable t, Eq a) => String -> a -> t a -> Assertion
    assertNotFoundIn what collateralOref outs =
      assertBool (what <> " should not contain collateral") (isNothing $ find (== collateralOref) outs)

    assertCollateralInDistribution :: TxOutRef -> [(TxOutRef, Ledger.Tx.ChainIndexTxOut)] -> Assertion
    assertCollateralInDistribution collateralOref utxs =
      let colalteral = lookup collateralOref utxs
       in if isJust colalteral
            then pure ()
            else
              assertFailure
                "Collateral UTxO not found in mock utxos distribution \
                \ - should not happen. Interrupting test with failure."
