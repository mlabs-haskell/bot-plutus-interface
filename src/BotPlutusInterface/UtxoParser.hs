module BotPlutusInterface.UtxoParser (
  feeParser,
  tokenNameParser,
) where

import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.ByteString.Char8 (isSpace)
import Data.Attoparsec.Text (
  Parser,
  char,
  choice,
  decimal,
  option,
  signed,
  skipSpace,
  string,
  takeWhile,
 )
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  TokenName (..),
 )
import PlutusTx.Builtins (toBuiltin)
import Relude hiding (takeWhile)

tokenNameParser :: Parser TokenName
tokenNameParser = do
  option "" tokenName
  where
    tokenName = do
      void $ char '.'
      void $ optional $ string "0x"
      TokenName <$> decodeHash (takeWhile (not . isSpace))

decodeHash :: Parser Text -> Parser BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . toBuiltin) (tryDecode parsed)

feeParser :: Parser Integer
feeParser =
  choice [prefixed, suffixed]
  where
    prefixed =
      void "Lovelace" *> skipSpace *> signed decimal
    suffixed =
      signed decimal <* skipSpace <* void "Lovelace"
