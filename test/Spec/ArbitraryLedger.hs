{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.ArbitraryLedger () where

import Data.ByteString (ByteString)
import Ledger (Address, DatumHash, PaymentPubKey, PubKey, StakePubKey, TxId, Validator, mkValidatorScript, pubKeyAddress)
import Ledger.Value qualified as Ledger
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (LedgerBytes)
import Plutus.V1.Ledger.Api qualified as LedgerBytes
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Instances ()
import Prelude

instance Arbitrary PlutusTx.BuiltinByteString where
  arbitrary = PlutusTx.toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary LedgerBytes where
  arbitrary = LedgerBytes.fromBytes <$> arbitrary

instance Arbitrary Ledger.CurrencySymbol where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Ledger.TokenName where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Address where
  arbitrary = oneof [Ledger.pubKeyAddress <$> arbitrary <*> arbitrary, mkValidatorAddress <$> arbitrary]

instance Arbitrary PaymentPubKey where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary StakePubKey where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PubKey where
  arbitrary = genericArbitrary
  shrink = genericShrink

acceptingValidator :: Ledger.Validator
acceptingValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [||(\_ _ _ -> ())||])

instance Arbitrary Ledger.Validator where
  arbitrary = pure acceptingValidator

instance Arbitrary Ledger.DatumHash where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary TxId where
  arbitrary = genericArbitrary
  shrink = genericShrink
