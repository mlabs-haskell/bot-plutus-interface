{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.RandomLedger (
  randomAssetClasses,
  randomValue,
  randomTxOut,
  randomTxOuts,
  randomTxOutRef,
  randomTxOutRefs,
) where

import Plutus.PAB.Arbitrary ()

import Control.Lens (folded, (%~), (^..))
import Data.List.Extra (mconcatMap)
import Data.Set qualified as Set
import Ledger.Tx (TxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass (AssetClass), Value)
import Ledger.Value qualified as Value
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Relude

deriving newtype instance Arbitrary AssetClass

randomAssetClasses :: Int -> Gen (Set AssetClass)
randomAssetClasses n = Set.fromList <$> replicateM n (arbitrary @AssetClass)

randomValue :: Int -> Set AssetClass -> Gen Value
randomValue samplesize assetclasses =
  do
    selectedAc <-
      Set.fromList
        <$> replicateM
          samplesize
          (assetclasses ^.. folded & id %~ elements)

    amounts <- replicateM (length selectedAc) (toInteger <$> arbitrary @Natural)

    pure $
      mconcatMap (uncurry Value.assetClassValue) $
        zip (Set.toList selectedAc) amounts

randomTxOut :: Int -> Set AssetClass -> Gen TxOut
randomTxOut samplesize assetclasses =
  do
    addr <- arbitrary
    value <- randomValue samplesize assetclasses
    datumhash <- arbitrary

    pure (TxOut addr value datumhash)

randomTxOuts :: Int -> Int -> Set AssetClass -> Gen [TxOut]
randomTxOuts numTxOuts samplesize =
  replicateM numTxOuts . randomTxOut samplesize

randomTxOutRef :: Gen TxOutRef
randomTxOutRef =
  do
    txId <- arbitrary
    txIdx <- toInteger <$> arbitrary @Natural
    pure (TxOutRef txId txIdx)

randomTxOutRefs :: Int -> Gen [TxOutRef]
randomTxOutRefs n = replicateM n randomTxOutRef
