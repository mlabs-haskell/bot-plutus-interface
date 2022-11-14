{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.RandomLedger (
  randomAssetClasses,
  randomValue,
  randomTxOut,
  randomTxOuts,
  randomTxOutRef,
  randomTxOutRefs,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Control.Lens (folded, (%~), (&), (^..))
import Control.Monad (replicateM)
import Data.Either (fromRight)
import Data.List.Extra (mconcatMap)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)
import Ledger.Tx (TxOut (..), TxOutRef (..))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Value (AssetClass (AssetClass), Value)
import Ledger.Value qualified as Value
import Plutus.PAB.Arbitrary ()
import Plutus.V2.Ledger.Api (OutputDatum (..))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Prelude

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
randomTxOut samplesize assetclasses = do
  addr <- arbitrary
  value <- randomValue samplesize assetclasses
  datumHash <- arbitrary

  let addr' = fromRight (error "Couldn't convert address") $ CardanoAPI.toCardanoAddressInEra (C.Testnet (C.NetworkMagic 9)) addr
      value' = fromRight (error "Couldn't convert value") $ CardanoAPI.toCardanoValue value
      datumHash' = fromRight (error "Couldn't convert datum hash") $ CardanoAPI.toCardanoTxOutDatum (OutputDatumHash datumHash)

  pure $ TxOut $ C.TxOut addr' (C.TxOutValue C.MultiAssetInBabbageEra value') datumHash' CS.ReferenceScriptNone

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
