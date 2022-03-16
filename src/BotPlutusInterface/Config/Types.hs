module BotPlutusInterface.Config.Types (
  ToValue (..),
  sectionWithDefault,
  sectionWithDefault',
  withNamePrefixSpec,
  serialize,
  deserialize',
  deserialize,
) where

import Config
import Config.Schema
import Config.Schema.Load.Error
import Control.Exception (displayException)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.PrettyPrint (Style (..), render, renderStyle, style)
import Prelude

class ToValue a where
  toValue :: a -> Value ()

sectionWithDefault :: HasSpec a => a -> Text -> Text -> SectionsSpec a
sectionWithDefault def_ section =
  sectionWithDefault' def_ section anySpec

sectionWithDefault' :: a -> Text -> ValueSpec a -> Text -> SectionsSpec a
sectionWithDefault' def_ section spec desc =
  fromMaybe def_ <$> optSection' section spec desc

withNamePrefixSpec :: Text -> ValueSpec a -> ValueSpec a
withNamePrefixSpec prefox spec = customSpec prefox spec Right

serialize :: (ToValue a) => a -> String
serialize = renderStyle style {lineLength = 200} . pretty . toValue

deserialize :: (HasSpec a) => String -> Either String a
deserialize = deserialize' anySpec

deserialize' :: ValueSpec a -> String -> Either String a
deserialize' spec s = case parse $ Text.pack s of
  Left e -> Left $ displayException e
  Right value -> first (render . prettyValueSpecMismatch) $ loadValue spec value