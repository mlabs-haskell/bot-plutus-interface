{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config.Ledger () where

import BotPlutusInterface.Config.Base ()
import BotPlutusInterface.Config.Types (ToValue (..))
import BotPlutusInterface.Types ()
import Config
import Config.Schema
import Data.String
import Data.String.ToString
import Data.Text qualified as Text
import Ledger.TimeSlot (SlotConfig (..))
import Plutus.V1.Ledger.Api (POSIXTime (..), fromBuiltin)
import Wallet.API (PubKeyHash (..))
import Prelude

instance ToValue PubKeyHash where
  toValue = Text () . Text.pack . toString . fromBuiltin . getPubKeyHash

instance HasSpec PubKeyHash where
  anySpec = pubKeyHashSpec

pubKeyHashSpec :: ValueSpec PubKeyHash
pubKeyHashSpec = PubKeyHash . fromString <$> stringSpec

instance ToValue POSIXTime where
  toValue = toValue . getPOSIXTime

instance ToValue SlotConfig where
  toValue (SlotConfig scSlotLength scSlotZeroTime) =
    Sections
      ()
      [ Section () "slotLength" $ toValue scSlotLength
      , Section () "slotZeroTime" $ toValue scSlotZeroTime
      ]

instance HasSpec SlotConfig where
  anySpec = slotConfigSpec

slotConfigSpec :: ValueSpec SlotConfig
slotConfigSpec = sectionsSpec "slotConfig - configure the length (ms) of one slot and the beginning of the first slot." $ do
  scSlotLength <-
    reqSection'
      "slotLength"
      integerSpec
      "Length (number of milliseconds) of one slot"
  scSlotZeroTime <-
    reqSection'
      "slotZeroTime"
      (POSIXTime <$> integerSpec)
      "Beginning of slot 0 (in milliseconds)"
  pure SlotConfig {..}