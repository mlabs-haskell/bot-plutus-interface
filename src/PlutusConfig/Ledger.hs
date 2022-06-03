{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module PlutusConfig.Ledger () where

import Config (Section (Section), Value (Sections, Text))
import Config.Schema (
  HasSpec (anySpec),
  ValueSpec,
  integerSpec,
  reqSection',
  sectionsSpec,
  stringSpec,
 )
import Data.String (fromString)
import Data.Text (Text)
import Ledger (POSIXTime (..), PubKeyHash, StakePubKeyHash (..))
import Ledger.TimeSlot (SlotConfig (SlotConfig), scSlotLength, scSlotZeroTime)
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

instance ToValue SlotConfig where
  toValue (SlotConfig scSlotLength scSlotZeroTime) =
    Sections
      ()
      [ Section () "length" $ toValue scSlotLength
      , Section () "zeroTime" $ toValue scSlotZeroTime
      ]

instance HasSpec SlotConfig where
  anySpec = slotConfigSpec

slotConfigSpec :: ValueSpec SlotConfig
slotConfigSpec = sectionsSpec "SlotConfig configuration" $ do
  scSlotLength <-
    reqSection'
      "length"
      integerSpec
      "Length (number of milliseconds) of one slot"
  scSlotZeroTime <-
    reqSection'
      "zeroTime"
      (POSIXTime <$> integerSpec)
      "Beginning of slot 0 (in milliseconds)"
  pure SlotConfig {..}
