{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS -fno-warn-orphans  #-}

module PlutusConfig.Ledger () where

import Config (Value (Text))
import Config.Schema (HasSpec (anySpec), ValueSpec, stringSpec)
import Data.String (fromString)
import Data.Text (Text)
import Ledger (POSIXTime (..), PubKeyHash, StakePubKeyHash (..))
import PlutusConfig.Base ()
import PlutusConfig.Types (ToValue (toValue), withNamePrefixSpec)
import Prelude

instance ToValue PubKeyHash where
  toValue = Text () . fromString . show

instance HasSpec PubKeyHash where
  anySpec = pubKeyHashSpec "PubKeyHash"

pubKeyHashSpec :: Text -> ValueSpec PubKeyHash
pubKeyHashSpec name = fromString <$> withNamePrefixSpec name stringSpec

instance ToValue StakePubKeyHash where
  toValue = toValue . unStakePubKeyHash

instance HasSpec StakePubKeyHash where
  anySpec = StakePubKeyHash <$> pubKeyHashSpec "StakePubKeyHash"

instance ToValue POSIXTime where
  toValue = toValue . getPOSIXTime
