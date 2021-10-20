module MLabsPAB.Files (
  policyScriptFilePath,
  validatorScriptFilePath,
  writeAll,
  writePolicyScriptFile,
  redeemerJsonFilePath,
  writeRedeemerJsonFile,
  writeValidatorScriptFile,
  datumJsonFilePath,
  writeDatumJsonFile,
) where

import Cardano.Api (FileError, writeFileJSON, writeFileTextEnvelope)
import Cardano.Api.Shelley (
  PlutusScript (PlutusScriptSerialised),
  PlutusScriptV1,
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  fromPlutusData,
  scriptDataToJson,
 )
import Codec.Serialise qualified as Codec
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Value qualified as Value
import MLabsPAB.Types (PABConfig)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  DatumHash (..),
  MintingPolicy,
  Redeemer (getRedeemer),
  RedeemerHash (..),
  Script,
  Validator,
  ValidatorHash (..),
 )
import PlutusTx (ToData, toData)
import PlutusTx.Builtins (fromBuiltin)
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

datumJsonFilePath :: PABConfig -> DatumHash -> Text
datumJsonFilePath pabConf (DatumHash datumHash) =
  let h = encodeByteString $ fromBuiltin datumHash
   in pabConf.pcScriptFileDir <> "/datum-" <> h <> ".json"

redeemerJsonFilePath :: PABConfig -> RedeemerHash -> Text
redeemerJsonFilePath pabConf (RedeemerHash redeemerHash) =
  let h = encodeByteString $ fromBuiltin redeemerHash
   in pabConf.pcScriptFileDir <> "/redeemer-" <> h <> ".json"

-- | Compiles and writes a script file under the given folder
writePolicyScriptFile :: PABConfig -> MintingPolicy -> IO (Either (FileError ()) Text)
writePolicyScriptFile pabConf mintingPolicy =
  let script = serialiseScript $ Ledger.unMintingPolicyScript mintingPolicy
      filepath = policyScriptFilePath pabConf (Ledger.scriptCurrencySymbol mintingPolicy)
   in fmap (const filepath) <$> writeFileTextEnvelope (Text.unpack filepath) Nothing script

-- | Compiles and writes a script file under the given folder
writeValidatorScriptFile :: PABConfig -> Validator -> IO (Either (FileError ()) Text)
writeValidatorScriptFile pabConf validatorScript =
  let script = serialiseScript $ Ledger.unValidatorScript validatorScript
      filepath = validatorScriptFilePath pabConf (Ledger.validatorHash validatorScript)
   in fmap (const filepath) <$> writeFileTextEnvelope (Text.unpack filepath) Nothing script

writeAll ::
  PABConfig ->
  [MintingPolicy] ->
  [Validator] ->
  [Datum] ->
  [Redeemer] ->
  IO (Either (FileError ()) [Text])
writeAll pabConf policyScripts validatorScripts datums redeemers =
  sequence
    <$> mconcat
      [ mapM (writePolicyScriptFile pabConf) policyScripts
      , mapM (writeValidatorScriptFile pabConf) validatorScripts
      , mapM (writeDatumJsonFile pabConf) datums
      , mapM (writeRedeemerJsonFile pabConf) redeemers
      ]

serialiseScript :: Script -> PlutusScript PlutusScriptV1
serialiseScript =
  PlutusScriptSerialised
    . ShortByteString.toShort
    . LazyByteString.toStrict
    . Codec.serialise

writeDatumJsonFile :: PABConfig -> Datum -> IO (Either (FileError ()) Text)
writeDatumJsonFile pabConf datum =
  let json = dataToJson $ getDatum datum
      filepath = datumJsonFilePath pabConf (Ledger.datumHash datum)
   in fmap (const filepath) <$> writeFileJSON (Text.unpack filepath) json

writeRedeemerJsonFile :: PABConfig -> Redeemer -> IO (Either (FileError ()) Text)
writeRedeemerJsonFile pabConf redeemer =
  let json = dataToJson $ getRedeemer redeemer
      filepath = redeemerJsonFilePath pabConf (Ledger.redeemerHash redeemer)
   in fmap (const filepath) <$> writeFileJSON (Text.unpack filepath) json

dataToJson :: ToData a => a -> JSON.Value
dataToJson =
  scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData
