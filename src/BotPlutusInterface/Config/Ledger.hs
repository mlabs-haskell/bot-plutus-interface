{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config.Ledger () where

import BotPlutusInterface.Config.Base ()
import BotPlutusInterface.Config.Types (ToValue (toValue), withNamePrefixSpec)
import BotPlutusInterface.Types ()
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
import Ledger.TimeSlot (SlotConfig (SlotConfig), scSlotLength, scSlotZeroTime)
import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime), getPOSIXTime)
import Wallet.API (PubKeyHash)
import Prelude

instance ToValue PubKeyHash where
  toValue = Text () . fromString . show

instance HasSpec PubKeyHash where
  anySpec = pubKeyHashSpec

pubKeyHashSpec :: ValueSpec PubKeyHash
pubKeyHashSpec = fromString <$> withNamePrefixSpec "PubKeyHash" stringSpec

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
