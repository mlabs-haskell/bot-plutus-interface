module BotPlutusInterface.UtxoParser (
  chainIndexTxOutParser,
  feeParser,
  utxoParser,
  utxoMapParser,
) where

import Control.Applicative (many)
import Control.Monad (mzero, void)
import Data.Aeson.Extras (tryDecode)
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
  takeWhile,
  (<?>),
 )
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Ledger (Address (addressCredential))
import Ledger.Ada qualified as Ada
import Ledger.Scripts (DatumHash (..))
import Ledger.Tx (
  ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut),
  TxOutRef (..),
 )
import Ledger.TxId (TxId (..))
import Ledger.Value (AssetClass, TokenName, Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (..),
 )
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

  case addressCredential address of
    ScriptCredential validatorHash -> do
      datumHash <- datumHashParser <?> "DatumHash"
      pure $ ScriptChainIndexTxOut address (Left validatorHash) (Left datumHash) value
    PubKeyCredential _ -> do
      datumHashNoneParser <?> "DatumHash"
      pure $ PublicKeyChainIndexTxOut address value

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
      Value.tokenName . encodeUtf8 <$> takeWhile (/= ' ')

datumHashNoneParser :: Parser ()
datumHashNoneParser = "TxOutDatumNone" >> pure ()

datumHashParser :: Parser DatumHash
datumHashParser = do
  void "TxOutDatumHash"
  skipSpace
  void "ScriptDataInAlonzoEra"
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
