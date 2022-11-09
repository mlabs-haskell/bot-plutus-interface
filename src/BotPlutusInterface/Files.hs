{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.Files (
  policyScriptFilePath,
  DummyPrivKey (FromSKey, FromVKey),
  unDummyPrivateKey,
  validatorScriptFilePath,
  readPrivateKeys,
  signingKeyFilePath,
  txFilePath,
  txFileName,
  txIdToText,
  -- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
  -- metadataFilePath,
  writeAll,
  writePolicyScriptFile,
  redeemerJsonFilePath,
  referenceScriptFilePath,
  mkDummyPrivateKey,
  writeRedeemerJsonFile,
  writeValidatorScriptFile,
  datumJsonFilePath,
  skeyToDummyPrivKey,
  vkeyToDummyPrivKey,
  writeDatumJsonFile,
) where

import BotPlutusInterface.Effects (
  PABEffect,
  createDirectoryIfMissing,
  listDirectory,
  readFileTextEnvelope,
  writeFileJSON,
  writeFileTextEnvelope,
 )
import BotPlutusInterface.Types (PABConfig)
import Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey, AsVerificationKey),
  BabbageEra,
  FileError,
  HasTextEnvelope,
  Key (VerificationKey),
  PaymentKey,
  PlutusScriptV1,
  PlutusScriptV2,
  SigningKey,
  TxOut (TxOut),
  TxOutDatum (TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline, TxOutDatumNone),
  getVerificationKey,
  serialiseToRawBytes,
 )
import Cardano.Api.Shelley (
  PlutusScript (PlutusScriptSerialised),
  ReferenceScript (ReferenceScript),
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  fromPlutusData,
  scriptDataToJson,
  toPlutusData,
 )
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise qualified as Codec
import Control.Monad.Freer (Eff, Member)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Either.Combinators (mapLeft)
import Data.Kind (Type)
import Data.List (isPrefixOf, nub, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Crypto (PubKey (PubKey), PubKeyHash (PubKeyHash))
import Ledger.Crypto qualified as Crypto
import Ledger.Tx (Tx, TxOut (getTxOut))
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (fromCardanoScriptInAnyLang)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.Scripts (Versioned (Versioned))
import Plutus.Script.Utils.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  DatumHash (..),
  LedgerBytes (LedgerBytes),
  MintingPolicy,
  Redeemer (getRedeemer),
  RedeemerHash (..),
  Script,
  Validator,
  ValidatorHash (..),
  toBuiltin,
 )
import PlutusTx (ToData, dataToBuiltinData, toData)
import PlutusTx.Builtins (fromBuiltin)
import System.FilePath (takeExtension, (</>))
import Prelude

-- | Filename of a minting policy script
policyScriptFilePath :: PABConfig -> CurrencySymbol -> Text
policyScriptFilePath pabConf curSymbol =
  let c = encodeByteString $ fromBuiltin $ Value.unCurrencySymbol curSymbol
   in pabConf.pcScriptFileDir <> "/policy-" <> c <> ".plutus"

-- | Path and filename of a validator script
validatorScriptFilePath :: PABConfig -> ValidatorHash -> Text
validatorScriptFilePath pabConf (ValidatorHash valHash) =
  let h = encodeByteString $ fromBuiltin valHash
   in pabConf.pcScriptFileDir <> "/validator-" <> h <> ".plutus"

referenceScriptFilePath :: PABConfig -> Ledger.ScriptHash -> Text
referenceScriptFilePath pabConf scriptHash =
  let h = encodeByteString $ fromBuiltin $ Ledger.getScriptHash scriptHash
   in pabConf.pcScriptFileDir <> "/reference-script-" <> h <> ".plutus"

datumJsonFilePath :: PABConfig -> DatumHash -> Text
datumJsonFilePath pabConf (DatumHash datumHash) =
  let h = encodeByteString $ fromBuiltin datumHash
   in pabConf.pcScriptFileDir <> "/datum-" <> h <> ".json"

redeemerJsonFilePath :: PABConfig -> RedeemerHash -> Text
redeemerJsonFilePath pabConf (RedeemerHash redeemerHash) =
  let h = encodeByteString $ fromBuiltin redeemerHash
   in pabConf.pcScriptFileDir <> "/redeemer-" <> h <> ".json"

signingKeyFilePath :: PABConfig -> PubKeyHash -> Text
signingKeyFilePath pabConf (PubKeyHash pubKeyHash) =
  let h = encodeByteString $ fromBuiltin pubKeyHash
   in pabConf.pcSigningKeyFileDir <> "/signing-key-" <> h <> ".skey"

txFilePath :: PABConfig -> Text -> Tx.TxId -> Text
txFilePath pabConf ext txId = pabConf.pcTxFileDir <> "/" <> txFileName txId ext

txFileName :: Tx.TxId -> Text -> Text
txFileName txId ext = "tx-" <> txIdToText txId <> "." <> ext

txIdToText :: Tx.TxId -> Text
txIdToText = encodeByteString . fromBuiltin . Tx.getTxId

-- TODO: Removed for now, as the main iohk branch doesn't support metadata yet

{- | Path of stored metadata files
 metadataFilePath :: PABConfig -> BuiltinByteString -> Text
 metadataFilePath pabConf (BuiltinByteString meta) =
   let h = encodeByteString $ blake2b meta
    in pabConf.pcMetadataDir <> "/metadata-" <> h <> ".json"
-}

-- | Compiles and writes a script file under the given folder
writePolicyScriptFile ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Versioned MintingPolicy ->
  Eff effs (Either (FileError ()) Text)
writePolicyScriptFile pabConf mintingPolicy =
  let script = Ledger.unMintingPolicyScript <$> mintingPolicy
      filepath = policyScriptFilePath pabConf (ScriptUtils.scriptCurrencySymbol mintingPolicy)
   in writeScriptEnvelope @w script filepath

-- | Compiles and writes a script file under the given folder
writeValidatorScriptFile ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Versioned Validator ->
  Eff effs (Either (FileError ()) Text)
writeValidatorScriptFile pabConf validatorScript =
  let script = Ledger.unValidatorScript <$> validatorScript
      filepath = validatorScriptFilePath pabConf (ScriptUtils.validatorHash validatorScript)
   in writeScriptEnvelope @w script filepath

writeReferenceScriptFile ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Versioned Script ->
  Eff effs (Either (FileError ()) Text)
writeReferenceScriptFile pabConf script =
  let filepath = referenceScriptFilePath pabConf (ScriptUtils.scriptHash script)
   in writeScriptEnvelope @w script filepath

-- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
-- -- | Writes metadata file under the given folder
-- writeMetadataFile ::
--   forall (w :: Type) (effs :: [Type -> Type]).
--   Member (PABEffect w) effs =>
--   PABConfig ->
--   BuiltinByteString ->
--   Eff effs (Either (FileError ()) Text)
-- writeMetadataFile pabConf metadata =
--   let filepath = metadataFilePath pabConf metadata
--    in const filepath <<$>> writeFileRaw @w (Text.unpack filepath) metadata

txMintingPolicies :: Tx.Tx -> [Versioned MintingPolicy]
txMintingPolicies tx = mapMaybe (Ledger.lookupMintingPolicy $ Tx.txScripts tx) $ Map.keys $ Tx.txMintingWitnesses tx

txValidatorInputs :: Tx.Tx -> [(Maybe (Versioned Validator), Redeemer, Datum)]
txValidatorInputs tx = mapMaybe (fromTxInputType . Tx.txInputType) $ Tx.txInputs tx <> Tx.txReferenceInputs tx
  where
    fromTxInputType :: Tx.TxInputType -> Maybe (Maybe (Versioned Validator), Redeemer, Datum)
    fromTxInputType (Tx.TxScriptAddress r eVHash mayDatHash) = do
      mValidator <- either (Just <$> Tx.lookupValidator (Tx.txScripts tx)) (const $ pure Nothing) eVHash
      datHash <- mayDatHash
      datum <- Tx.lookupDatum tx datHash
      pure (mValidator, r, datum)
    fromTxInputType _ = Nothing

txReferenceScripts :: Tx.Tx -> [Versioned Script]
txReferenceScripts tx = catMaybes $ getVersionedScript . Tx.txOutReferenceScript <$> Tx.txOutputs tx
  where
    getVersionedScript :: ReferenceScript BabbageEra -> Maybe (Versioned Script)
    getVersionedScript (ReferenceScript _ s) = fromCardanoScriptInAnyLang s
    getVersionedScript _ = Nothing

txOutpuDatums :: Tx.Tx -> [Datum]
txOutpuDatums tx = do
  TxOut _ _ dat _ <- getTxOut <$> Tx.txOutputs tx
  case dat of
    TxOutDatumNone -> []
    TxOutDatumHash _ sdHash -> maybe [] return $ Tx.lookupDatum tx (DatumHash . toBuiltin . serialiseToRawBytes $ sdHash)
    TxOutDatumInline _ sd -> return . Ledger.Datum . dataToBuiltinData . toPlutusData $ sd
    TxOutDatumInTx _ sd -> return . Ledger.Datum . dataToBuiltinData . toPlutusData $ sd

-- | Write to disk all validator scripts, datums and redemeers appearing in the tx
writeAll ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Tx ->
  Eff effs (Either (FileError ()) [Text])
writeAll pabConf tx = do
  createDirectoryIfMissing @w False (Text.unpack pabConf.pcScriptFileDir)
  -- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
  -- createDirectoryIfMissing @w False (Text.unpack pabConf.pcMetadataDir)

  let (mValidatorScripts, redeemers, datums) = unzip3 $ txValidatorInputs tx
      validatorScripts = catMaybes mValidatorScripts
      policyScripts = txMintingPolicies tx
      allDatums = nub $ datums <> Map.elems (Tx.txData tx) <> txOutpuDatums tx
      allRedeemers = redeemers <> Map.elems (Tx.txRedeemers tx)
      allRefScripts = txReferenceScripts tx

  results <-
    sequence $
      mconcat
        [ map (writePolicyScriptFile @w pabConf) policyScripts
        , map (writeValidatorScriptFile @w pabConf) validatorScripts
        , map (writeDatumJsonFile @w pabConf) allDatums
        , map (writeRedeemerJsonFile @w pabConf) allRedeemers
        , map (writeReferenceScriptFile @w pabConf) allRefScripts
        -- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
        -- , map (writeMetadataFile @w pabConf) (maybeToList $ Tx.txMetadata tx)
        ]

  pure $ sequence results

readPrivateKeys ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Eff effs (Either Text (Map PubKeyHash DummyPrivKey))
readPrivateKeys pabConf = do
  files <- listDirectory @w $ Text.unpack pabConf.pcSigningKeyFileDir
  privKeys <- catMaybes <$> mapM readKey files
  pure $ toPrivKeyMap <$> sequence privKeys
  where
    readKey filename =
      let fullPath = Text.unpack pabConf.pcSigningKeyFileDir </> filename
       in case takeExtension filename of
            ".vkey" ->
              guardPaymentKey paymentVKeyPrefix filename
                <$> readVerificationKey @w fullPath
            ".skey" ->
              guardPaymentKey paymentSKeyPrefix filename
                <$> readSigningKey @w fullPath
            _ -> pure Nothing

    paymentVKeyPrefix = "verification-key"
    paymentSKeyPrefix = "signing-key"

    {- this filtering ensures that only payment keys are read,
     it allows to store other types of keys in the same drirectory if required
     by altering filename prefix
    -}
    guardPaymentKey prefix filename =
      if prefix `isPrefixOf` filename then Just else const Nothing

    toPrivKeyMap :: [DummyPrivKey] -> Map PubKeyHash DummyPrivKey
    toPrivKeyMap =
      foldl
        ( \pKeyMap pKey ->
            let pkh = Crypto.pubKeyHash $ Crypto.toPublicKey $ unDummyPrivateKey pKey
             in Map.insert pkh pKey pKeyMap
        )
        Map.empty
        . sortOn keyPriority

    keyPriority :: DummyPrivKey -> Int
    keyPriority (FromSKey _) = 1
    keyPriority (FromVKey _) = 0

data DummyPrivKey
  = FromSKey Crypto.XPrv
  | FromVKey Crypto.XPrv

unDummyPrivateKey :: DummyPrivKey -> Crypto.XPrv
unDummyPrivateKey (FromSKey key) = key
unDummyPrivateKey (FromVKey key) = key

readSigningKey ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  FilePath ->
  Eff effs (Either Text DummyPrivKey)
readSigningKey filePath = do
  pKey <- mapLeft (Text.pack . show) <$> readFileTextEnvelope @w (AsSigningKey AsPaymentKey) filePath
  pure $ skeyToDummyPrivKey =<< pKey

readVerificationKey ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  FilePath ->
  Eff effs (Either Text DummyPrivKey)
readVerificationKey filePath = do
  pKey <- mapLeft (Text.pack . show) <$> readFileTextEnvelope @w (AsVerificationKey AsPaymentKey) filePath
  pure $ vkeyToDummyPrivKey =<< pKey

vkeyToDummyPrivKey :: VerificationKey PaymentKey -> Either Text DummyPrivKey
vkeyToDummyPrivKey =
  fmap FromVKey . vkeyToDummyPrivKey'

skeyToDummyPrivKey :: SigningKey PaymentKey -> Either Text DummyPrivKey
skeyToDummyPrivKey =
  fmap FromSKey . vkeyToDummyPrivKey' . getVerificationKey

{- | Warning! This implementation is not correct!
 This private key is derived from a normal signing key which uses a 32 byte private key compared
 to the extended key which is 64 bytes. Also, the extended key includes a chain index value

 This key's sole purpose is to be able to derive a public key from it, which is then used for
 mapping to a signing key file for the CLI
-}
vkeyToDummyPrivKey' :: VerificationKey PaymentKey -> Either Text Crypto.XPrv
vkeyToDummyPrivKey' =
  mkDummyPrivateKey . PubKey . LedgerBytes . toBuiltin . serialiseToRawBytes

mkDummyPrivateKey :: PubKey -> Either Text Crypto.XPrv
mkDummyPrivateKey (PubKey (LedgerBytes pubkey)) =
  let dummyPrivKey = ByteString.replicate 32 0
      dummyPrivKeySuffix = ByteString.replicate 32 0
      dummyChainCode = ByteString.replicate 32 1
      pubkeyBS = fromBuiltin pubkey
   in mapLeft Text.pack $
        Crypto.xprv $
          mconcat [dummyPrivKey, dummyPrivKeySuffix, pubkeyBS, dummyChainCode]

serialiseScript :: forall (lang :: Type). Script -> PlutusScript lang
serialiseScript =
  PlutusScriptSerialised
    . ShortByteString.toShort
    . LazyByteString.toStrict
    . Codec.serialise

writeScriptEnvelope ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Versioned Script ->
  Text ->
  Eff effs (Either (FileError ()) Text)
writeScriptEnvelope vScript filepath =
  fmap (const filepath)
    <$> case vScript of
      (Versioned s ScriptUtils.PlutusV1) -> writeScriptEnvelope' $ serialiseScript @PlutusScriptV1 s
      (Versioned s ScriptUtils.PlutusV2) -> writeScriptEnvelope' $ serialiseScript @PlutusScriptV2 s
  where
    writeScriptEnvelope' ::
      forall (a :: Type).
      HasTextEnvelope a =>
      a ->
      Eff effs (Either (FileError ()) ())
    writeScriptEnvelope' = writeFileTextEnvelope @w (Text.unpack filepath) Nothing

writeDatumJsonFile ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Datum ->
  Eff effs (Either (FileError ()) Text)
writeDatumJsonFile pabConf datum =
  let json = dataToJson $ getDatum datum
      filepath = datumJsonFilePath pabConf (ScriptUtils.datumHash datum)
   in fmap (const filepath) <$> writeFileJSON @w (Text.unpack filepath) json

writeRedeemerJsonFile ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Redeemer ->
  Eff effs (Either (FileError ()) Text)
writeRedeemerJsonFile pabConf redeemer =
  let json = dataToJson $ getRedeemer redeemer
      filepath = redeemerJsonFilePath pabConf (ScriptUtils.redeemerHash redeemer)
   in fmap (const filepath) <$> writeFileJSON @w (Text.unpack filepath) json

dataToJson :: forall (a :: Type). ToData a => a -> JSON.Value
dataToJson =
  scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData
