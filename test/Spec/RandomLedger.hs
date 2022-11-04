{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.RandomLedger (
  randomAssetClasses,
  randomValue,
  randomTxOut,
  randomTxOuts,
  randomTxOutRef,
  randomTxOutRefs,
) where

import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (ReferenceScript (ReferenceScriptNone))
import Control.Lens (folded, (%~), (&), (^..))
import Control.Monad (replicateM)
import Data.ByteString (pack)
import Data.Either.Combinators (fromRight)
import Data.List.Extra (mconcatMap)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)
import Ledger.Tx (TxOut (..), TxOutRef (..))
import Ledger.Tx.CardanoAPI.Internal (toCardanoAddressInEra, toCardanoScriptDataHash, toCardanoValue)
import Ledger.Value (AssetClass (AssetClass), CurrencySymbol, TokenName, Value)
import Ledger.Value qualified as Value
import Spec.MockContract (testingNetwork)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, chooseInt, elements)
import Prelude

-- instances in Plutus.PAB.Instances do not enforce length constraints

genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = Value.currencySymbol . pack <$> replicateM 28 arbitrary

genTokenName :: Gen TokenName
genTokenName = do
  len <- chooseInt (0, 32)
  Value.tokenName . pack <$> replicateM len arbitrary

instance Arbitrary AssetClass where
  arbitrary = (\cs tn -> AssetClass (cs, tn)) <$> genCurrencySymbol <*> genTokenName

randomAssetClasses :: Int -> Gen (Set AssetClass)
randomAssetClasses n = Set.fromList <$> replicateM n (arbitrary @AssetClass)

randomValue :: Int -> Set AssetClass -> Gen Value
randomValue sampleSize assetClasses = do
  selectedAc <-
    Set.fromList
      <$> replicateM
        sampleSize
        (assetClasses ^.. folded & id %~ elements)

  amounts <- replicateM (length selectedAc) (toInteger <$> arbitrary @Natural)

  pure $
    mconcatMap (uncurry Value.assetClassValue) $
      zip (Set.toList selectedAc) amounts

randomTxOut :: Int -> Set AssetClass -> Gen TxOut
randomTxOut sampleSize assetClasses = do
  addr <- fromRight undefined . toCardanoAddressInEra testingNetwork <$> arbitrary
  value <- fromRight undefined . toCardanoValue <$> randomValue sampleSize assetClasses
  datumHash <- fromRight undefined . toCardanoScriptDataHash <$> arbitrary

  pure
    ( TxOut $
        CApi.TxOut
          addr
          (CApi.TxOutValue CApi.MultiAssetInBabbageEra value)
          (CApi.TxOutDatumHash CApi.ScriptDataInBabbageEra datumHash)
          ReferenceScriptNone
    )

randomTxOuts :: Int -> Int -> Set AssetClass -> Gen [TxOut]
randomTxOuts numTxOuts sampleSize =
  replicateM numTxOuts . randomTxOut sampleSize

randomTxOutRef :: Gen TxOutRef
randomTxOutRef = do
  txId <- arbitrary
  txIdx <- toInteger <$> arbitrary @Natural
  pure (TxOutRef txId txIdx)

randomTxOutRefs :: Int -> Gen [TxOutRef]
randomTxOutRefs n = replicateM n randomTxOutRef
