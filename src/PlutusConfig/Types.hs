{- |Flow:

 @
      Config.parse               Config.Schema.HasSpec a
 Text ------------> Config.Value ----------------------> a
                                                         |
                                                         v
 Text <----------------------- Config.Value <----------- a
       Text.PrettyPrint.pretty                ToValue a
 @

Notes: input and output text may have minor differences. E.g., different cases
or different sequences in section.
-}
module PlutusConfig.Types (
  -- *Serialization
  ToValue (toValue),

  -- *Deserialization
  HasSpec (anySpec),
  withNamePrefixSpec,
  sectionWithDefault,
  sectionWithDefault',

  -- *Marshaling
  serialize,
  deserialize',
  deserialize,
) where

import Config (Value, parse, pretty)
import Config.Schema (
  HasSpec (anySpec),
  SectionsSpec,
  ValueSpec,
  customSpec,
  loadValue,
  optSection',
 )
import Config.Schema.Load.Error (prettyValueSpecMismatch)
import Data.Text qualified as Text
import Relude
import Text.PrettyPrint (Style (lineLength), render, renderStyle, style)

class ToValue a where
  toValue :: a -> Value ()

sectionWithDefault ::
  forall a.
  (ToValue a, HasSpec a) =>
  a ->
  Text ->
  Text ->
  SectionsSpec a
sectionWithDefault def_ section =
  sectionWithDefault' def_ section anySpec

sectionWithDefault' ::
  forall a.
  ToValue a =>
  a ->
  Text ->
  ValueSpec a ->
  Text ->
  SectionsSpec a
sectionWithDefault' def_ section spec desc =
  let defStr = Text.pack $ render $ pretty $ toValue def_
      defHelp =
        if Text.isInfixOf "\n" defStr
          then " (see default in example)"
          else " (default: " <> defStr <> ")"
   in fromMaybe def_ <$> optSection' section spec (desc <> defHelp)

withNamePrefixSpec :: forall a. Text -> ValueSpec a -> ValueSpec a
withNamePrefixSpec prefox spec = customSpec prefox spec Right

serialize :: forall a. ToValue a => a -> String
serialize = renderStyle style {lineLength = 200} . pretty . toValue

deserialize :: forall a. HasSpec a => String -> Either String a
deserialize = deserialize' anySpec

deserialize' :: forall a. ValueSpec a -> String -> Either String a
deserialize' spec s = case parse $ Text.pack s of
  Left e -> Left $ displayException e
  Right value -> first (render . prettyValueSpecMismatch) $ loadValue spec value
