module Spec.PlutusConfig.Misc (serializeDeserialize, serializeDeserialize') where

import Config.Schema (HasSpec (anySpec), ValueSpec)
import PlutusConfig.Cardano.Api ()
import PlutusConfig.Cardano.Api.Shelley ()
import PlutusConfig.Ledger ()
import PlutusConfig.Types (ToValue, deserialize', serialize)
import Prelude

serializeDeserialize :: (ToValue a, HasSpec a) => a -> Either String a
serializeDeserialize = serializeDeserialize' anySpec

serializeDeserialize' :: (ToValue a) => ValueSpec a -> a -> Either String a
serializeDeserialize' spec = deserialize' spec . serialize
