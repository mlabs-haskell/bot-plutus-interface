module BotPlutusInterface.UtxoParser (
  chainIndexTxOutParser,
  feeParser,
  utxoParser,
  utxoMapParser,
  tokenNameParser,
) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (mzero, void)
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.ByteString.Char8 (isSpace)
import Data.Attoparsec.Text (
  Parser,
  char,
  choice,
  count,
  decimal,
  inClass,
  isEndOfLine,
  option,
  sepBy,
  signed,
  skipSpace,
  skipWhile,
  string,
  takeWhile,
  (<?>),
 )
import Data.Functor (($>))
import Data.Text (Text)
import Ledger (Address)
import Ledger.Ada qualified as Ada
import Ledger.Scripts (DatumHash (..))
import Ledger.Tx (TxId (..), TxOutRef (..))
import Ledger.Value (AssetClass, Value)
import Ledger.Value qualified as Value
import Plutus.ChainIndex.Tx (ChainIndexTxOut (ChainIndexTxOut))
import Plutus.ChainIndex.Types (ReferenceScript (ReferenceScriptNone))
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  CurrencySymbol (..),
  TokenName (..),
 )
import Plutus.V2.Ledger.Api (OutputDatum (NoOutputDatum, OutputDatumHash))
import PlutusTx.Builtins (toBuiltin)
import Prelude hiding (takeWhile)

utxoMapParser :: Address -> Parser [(TxOutRef, ChainIndexTxOut)]
utxoMapParser address = do
  skipLine 2
  many (utxoParser address)

skipLine :: Int -> Parser ()
skipLine n =
  void $
    count n $ do
      skipWhile (not . isEndOfLine)
      skipWhile isEndOfLine

utxoParser :: Address -> Parser (TxOutRef, ChainIndexTxOut)
utxoParser address =
  (,) <$> (txOutRefParser <?> "TxOutRef") <* skipSpace
    <*> (chainIndexTxOutParser address <?> "ChainIndexTxOut") <* skipWhile isEndOfLine

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= ' '))

  skipSpace
  txIx <- decimal
  pure $ TxOutRef txId txIx

chainIndexTxOutParser :: Address -> Parser ChainIndexTxOut
chainIndexTxOutParser address = do
  value <- mconcat <$> (valueParser <?> "Value") `sepBy` " + "
  void " + "

  outputDatum <- outputDatumParser <?> "OutputDatum"
  pure $
    ChainIndexTxOut
      address
      value
      outputDatum
      ReferenceScriptNone

valueParser :: Parser Value
valueParser = do
  amt <- signed decimal
  skipSpace
  assetClass <- assetClassParser <?> "AssetClass"
  pure $ Value.assetClassValue assetClass amt

assetClassParser :: Parser AssetClass
assetClassParser =
  choice [adaAssetClass, otherAssetClass]
  where
    adaAssetClass = Value.assetClass Ada.adaSymbol Ada.adaToken <$ "lovelace"
    otherAssetClass = do
      curSymbol <- CurrencySymbol <$> decodeHash (takeWhile (not . inClass " .")) <?> "CurrencySymbol"
      tokenname <- tokenNameParser <?> "TokenName"
      pure $ Value.assetClass curSymbol tokenname

tokenNameParser :: Parser TokenName
tokenNameParser = do
  option "" tokenName
  where
    tokenName = do
      void $ char '.'
      void $ optional $ string "0x"
      TokenName <$> decodeHash (takeWhile (not . isSpace))

-- TODO: Handle inline datums, if we need them here
outputDatumParser :: Parser OutputDatum
outputDatumParser =
  OutputDatumHash <$> datumHashParser
    <|> "TxOutDatumNone" $> NoOutputDatum

-- FIXME: will it fail for "TxOutDatumInline ..."?
datumHashParser :: Parser DatumHash
datumHashParser = do
  void "TxOutDatumHash"
  skipSpace
  void $ "ScriptDataInAlonzoEra" <|> "ScriptDataInBabbageEra"
  skipSpace
  char '\"' *> (DatumHash <$> decodeHash (takeWhile (/= '\"'))) <* char '\"'

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
