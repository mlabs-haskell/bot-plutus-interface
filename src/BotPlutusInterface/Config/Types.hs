module BotPlutusInterface.Config.Types (
  ToValue (..),
  optSectionFromDef,
  optSectionWithDef',
  optSectionFromDef',
  deserialize',
  deserialize,
  serialize,
) where

import Config
import Config.Schema
import Config.Schema.Load.Error
import Control.Exception (displayException)
import Data.Bifunctor (first)
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.PrettyPrint (Style (..), render, renderStyle, style)
import Prelude

class ToValue a where
  toValue :: a -> Value ()

optSectionWithDef' ::
  (Show b) =>
  b ->
  Text ->
  ValueSpec b ->
  Text ->
  SectionsSpec b
optSectionWithDef' def_ section spec desc =
  let defDesc = "(default: " <> Text.pack (show def_) <> ")"
      desc' = desc <> if Text.null desc then "" else " " <> defDesc
   in fromMaybe def_ <$> optSection' section spec desc'

optSectionFromDef ::
  (Default a, Show b, HasSpec b) =>
  (a -> b) ->
  Text ->
  Text ->
  SectionsSpec b
optSectionFromDef getter section =
  optSectionWithDef' (getter def) section anySpec

optSectionFromDef' ::
  (Default a, Show b) =>
  (a -> b) ->
  Text ->
  ValueSpec b ->
  Text ->
  SectionsSpec b
optSectionFromDef' getter =
  optSectionWithDef' (getter def)

serialize :: (ToValue a) => a -> String
serialize = renderStyle style {lineLength = 200} . pretty . toValue

deserialize :: (HasSpec a) => String -> Either String a
deserialize = deserialize' anySpec

deserialize' :: ValueSpec a -> String -> Either String a
deserialize' spec s = case parse $ Text.pack s of
  Left e -> Left $ displayException e
  Right value -> first (render . prettyValueSpecMismatch) $ loadValue spec value
