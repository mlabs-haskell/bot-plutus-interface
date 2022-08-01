{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.RandomLedger (
  allAssetClasses,
  randomValue,
  randomTxOut,
  randomTxOuts,
  randomTxOutRef,
) where

import Plutus.PAB.Arbitrary ()

import Control.Lens (folded, (%~), (&), (^..))
import Control.Monad (replicateM)
import Data.List.Extra (mconcatMap)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)
import Ledger.Tx (TxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass (AssetClass), Value)
import Ledger.Value qualified as Value
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, elements, forAll, listOf, property, resize)
import Prelude

deriving newtype instance Arbitrary AssetClass

allAssetClasses :: Int -> Gen (Set AssetClass)
allAssetClasses n = Set.fromList <$> resize n (listOf arbitrary)

randomValue :: Int -> Set AssetClass -> Gen Value
randomValue samplesize assetclasses =
  do
    selectedAc <-
      Set.fromList
        <$> replicateM
          samplesize
          (assetclasses ^.. folded & id %~ elements)

    amounts <- replicateM (length selectedAc) (toInteger <$> arbitrary @Natural)

    return $
      mconcatMap (uncurry Value.assetClassValue) $
        zip (Set.toList selectedAc) amounts

randomTxOut :: Int -> Set AssetClass -> Gen TxOut
randomTxOut samplesize assetclasses =
  do
    addr <- arbitrary
    value <- randomValue samplesize assetclasses
    datumhash <- arbitrary

    return (TxOut addr value datumhash)

randomTxOuts :: Int -> Int -> Set AssetClass -> Gen [TxOut]
randomTxOuts numTxOuts samplesize =
  replicateM numTxOuts . randomTxOut samplesize

randomTxOutRef :: Gen TxOutRef
randomTxOutRef =
  do
    txId <- arbitrary
    txIdx <- toInteger <$> arbitrary @Natural
    return (TxOutRef txId txIdx)
