{-# OPTIONS -fno-warn-orphans  #-}

module PlutusConfig.Base (
  -- *Serialization
  maybeSpec,
  caseAgnosticAtomSpec,
  customRationalSpec,
  portSpec,
  pathSpec,
  filepathSpec,
  textSpecViaJSON,

  -- *Deserialization
  toValueTextViaJSON,
  enumToAtom,
) where

import Config (
  Atom (MkAtom),
  Value (Atom, Number, Text),
  integerToNumber,
 )
import Config.Schema (
  HasSpec (anySpec),
  ValueSpec,
  anyAtomSpec,
  customSpec,
  naturalSpec,
  stringSpec,
  textSpec,
  (<!>),
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Bifunctor (first)
import Data.Ratio ((%))
import Data.String (fromString)
import Data.String.ToString (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import PlutusConfig.Types (ToValue (toValue), withNamePrefixSpec)
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import Text.Regex (matchRegex, mkRegex)
import Prelude

instance ToValue Bool where
  toValue = Atom () . MkAtom . Text.toLower . Text.pack . show

instance ToValue Natural where
  toValue x = Number () $ integerToNumber $ toInteger x

instance ToValue Integer where
  toValue x = Number () $ integerToNumber x

instance ToValue Int where
  toValue = toValue . toInteger

instance ToValue Text where
  toValue = Text ()

instance ToValue String where
  toValue = Text () . Text.pack

instance {-# OVERLAPS #-} HasSpec String where
  anySpec = stringSpec

instance ToValue a => ToValue (Maybe a) where
  toValue = maybe (Atom () "nothing") toValue

enumToAtom :: forall a. Show a => a -> Value ()
enumToAtom = Atom () . MkAtom . Text.toLower . Text.pack . show

maybeSpec :: forall a. ValueSpec a -> ValueSpec (Maybe a)
maybeSpec spec =
  Nothing <$ caseAgnosticAtomSpec "nothing"
    <!> Just <$> spec

instance ToValue Rational where
  toValue x = Text () $ Text.pack $ show x

customRationalSpec :: ValueSpec Rational
customRationalSpec =
  customSpec
    "Ratio number (\"1 % 2\") in"
    stringSpec
    ( \x -> case matchRegex ratioRE x of
        Just [n, d] ->
          let n' = read n
              d' = read d
           in if d' == 0
                then Left "denominator should not be zero"
                else Right $ n' % d'
        _ -> Left $ Text.pack "Ratio format: '1 % 2'"
    )
  where
    ratioRE = mkRegex "^ *([0-9]+) *% *([0-9]+) *$"

pathSpec :: ValueSpec Text
pathSpec = withNamePrefixSpec "path" anySpec

filepathSpec :: ValueSpec Text
filepathSpec = withNamePrefixSpec "filepath" anySpec

toValueTextViaJSON :: forall a. ToJSON a => a -> Value ()
toValueTextViaJSON = Text () . Text.pack . filter (/= '"') . toString . JSON.encode

textSpecViaJSON :: forall a. FromJSON a => Text -> ValueSpec a
textSpecViaJSON name =
  customSpec
    name
    textSpec
    ( \s -> case JSON.eitherDecode $ fromString $ wrap $ toString s of
        Left err -> Left $ "parse error: " <> fromString err
        Right res -> Right res
    )
  where
    wrap s = "\"" <> s <> "\""

instance ToValue BaseUrl where
  toValue = Text () . Text.pack . showBaseUrl

instance HasSpec BaseUrl where
  anySpec = baseUrlSpec

baseUrlSpec :: ValueSpec BaseUrl
baseUrlSpec =
  customSpec
    "url"
    anySpec
    (first (Text.pack . show) . parseBaseUrl . Text.unpack)

portSpec :: ValueSpec Port
portSpec = fromEnum <$> customSpec "port" naturalSpec Right

{- |Primitive specification for matching a particular atom in case way. E.g.:
 @caseAgnosticAtomSpec "Yes"@ will catch @yes@ and @YEs@.
-}
caseAgnosticAtomSpec :: Text -> ValueSpec ()
caseAgnosticAtomSpec tag = customSpec ("case insensitive `" <> tag <> "`") anyAtomSpec $
  \t ->
    if Text.toLower t == Text.toLower tag
      then Right ()
      else Left $ "should be " <> tag <> " (case insensitive), but actually: " <> t
