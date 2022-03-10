{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module BotPlutusInterface.Config where

import BotPlutusInterface.Types

import Cardano.Api (
  AnyPlutusScriptVersion,
  CostModel (..),
  EpochNo (EpochNo),
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  Lovelace (..),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (..),
  PraosNonce,
 )

import Cardano.Api.Shelley (ProtocolParameters (..), makePraosNonce)
import Config
import Config.Schema
import Control.Exception (displayException)
import Data.Aeson (eitherDecode, encode)
import Data.Bifunctor (first)
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio, (%))
import Data.String
import Data.String.ToString
import Data.Text (Text)
import Data.Text qualified as Data.Text.Encoding
import Data.Text qualified as Text
import Debug.Trace
import Ledger.TimeSlot (SlotConfig (..))
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import Text.PrettyPrint (render)
import Text.Regex
import Wallet.API (PubKeyHash (PubKeyHash))
import Prelude

instance (ToValue a) => ToValue (Maybe a) where toValue = maybe (Atom () "nothing") toValue

instance ToValue Natural where
  toValue x = Number () $ integerToNumber $ toInteger x

instance ToValue Integer where
  toValue x = Number () $ integerToNumber x

instance ToValue PraosNonce where
  toValue = Text () . Text.pack . show

instance ToValue EpochNo where
  toValue (EpochNo x) = Number () $ integerToNumber $ toInteger x

---------------

instance ToValue CLILocation where
  toValue Local = Text () "Local"
  toValue (Remote url) = Text () url

cliLocationSpec :: ValueSpec CLILocation
cliLocationSpec =
  Local <$ atomSpec "Local"
    <!> Remote <$> anySpec

---------------

instance ToValue LogLevel where
  toValue l = Text () $ Text.pack $ show l

logLevelSpec :: ValueSpec LogLevel
logLevelSpec =
  Error <$ atomSpec "Error"
    <!> Warn <$ atomSpec "Warn"
    <!> Notice <$ atomSpec "Notice"
    <!> Info <$ atomSpec "Info"
    <!> Debug <$ atomSpec "Debug"

---------------

instance ToValue Rational where
  toValue x = Text () $ Text.pack $ show x

customRationalSpec :: ValueSpec Rational
customRationalSpec =
  customSpec
    "Ratio"
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

---------------

instance ToValue NetworkId where
  toValue Mainnet = Atom () "mainnet"
  toValue (Testnet (NetworkMagic id)) = Number () $ integerToNumber $ toInteger id

networkIdSpec :: ValueSpec NetworkId
networkIdSpec =
  Mainnet <$ atomSpec "mainnet"
    <!> Testnet . NetworkMagic <$> anySpec

---------------

instance ToValue (Natural, Natural) where
  toValue (a, b) = List () [toValue a, toValue b]

protocolVersionSpec :: ValueSpec (Natural, Natural)
protocolVersionSpec =
  customSpec
    "protocol version"
    (listSpec naturalSpec)
    ( \case
        [major, minor] -> Right (major, minor)
        _ -> Left $ Text.pack "[MAJOR, MINOR]"
    )

---------------

instance ToValue Lovelace where
  toValue (Lovelace x) = Number () $ integerToNumber x

instance HasSpec Lovelace where
  anySpec = lovelaceSpec

lovelaceSpec :: ValueSpec Lovelace
lovelaceSpec = Lovelace . toInteger <$> naturalSpec

---------------

instance ToValue ExecutionUnitPrices where
  toValue (ExecutionUnitPrices priceExecutionSteps priceExecutionMemory) =
    Sections
      ()
      [ Section () "steps" $ toValue priceExecutionSteps
      , Section () "memory" $ toValue priceExecutionMemory
      ]

executionUnitPricesSpec :: ValueSpec ExecutionUnitPrices
executionUnitPricesSpec = sectionsSpec "The prices for 'ExecutionUnits' as a fraction of a 'Lovelace'." $ do
  priceExecutionSteps <- reqSection' "steps" customRationalSpec ""
  priceExecutionMemory <- reqSection' "memory" customRationalSpec ""
  pure ExecutionUnitPrices {..}

---------------

instance ToValue ExecutionUnits where
  toValue (ExecutionUnits executionSteps executionMemory) =
    Sections
      ()
      [ Section () "steps" $ toValue executionSteps
      , Section () "memory" $ toValue executionMemory
      ]

executionUnitsSpec :: ValueSpec ExecutionUnits
executionUnitsSpec =
  sectionsSpec "The units for how long a script executes for and how much memory it uses." $ do
    executionSteps <- reqSection "steps" "This corresponds roughly to the time to execute a script."
    executionMemory <- reqSection "memory" "This corresponds roughly to the peak memory used during script execution."
    pure ExecutionUnits {..}

---------------

instance ToValue (Map AnyPlutusScriptVersion CostModel) where
  toValue m = List () $ map pair $ Map.assocs m
    where
      pair (ver, cost) =
        Sections
          ()
          [ Section () "scriptVersion" $ toValue ver
          , Section () "costModel" $ toValue cost
          ]

versionCostModelSpec :: ValueSpec (Map AnyPlutusScriptVersion CostModel)
versionCostModelSpec = Map.fromList <$> listSpec pair
  where
    pair = sectionsSpec "" $ do
      ver <- reqSection' "scriptVersion" anyPlutusScriptVersionSpec ""
      cost <- reqSection' "costModel" costModelSpec ""
      pure (ver, cost)

---------------

instance ToValue AnyPlutusScriptVersion where
  toValue = Text () . fromString . filter (/= '"') . toString . encode

anyPlutusScriptVersionSpec :: ValueSpec AnyPlutusScriptVersion
anyPlutusScriptVersionSpec =
  customSpec
    "AnyPlutusScriptVersion"
    stringSpec
    (first Text.pack . eitherDecode . fromString . show)

---------------

instance ToValue CostModel where
  toValue (CostModel m) = Sections () $ map (\(k, i) -> Section () k (toValue i)) $ Map.assocs m

costModelSpec :: ValueSpec CostModel
costModelSpec = CostModel . Map.fromList <$> assocSpec integerSpec

---------------

{- ORMOLU_DISABLE -}
instance ToValue ProtocolParameters where
  toValue ProtocolParameters {..} =
    Sections
      ()
      [ Section () "protocolVersion"     $ toValue protocolParamProtocolVersion
      , Section () "decentralization"    $ toValue protocolParamDecentralization
      , Section () "extraPraosEntropy"   $ toValue protocolParamExtraPraosEntropy
      , Section () "maxBlockHeaderSize"  $ toValue protocolParamMaxBlockHeaderSize
      , Section () "maxBlockBodySize"    $ toValue protocolParamMaxBlockBodySize
      , Section () "maxTxSize"           $ toValue protocolParamMaxTxSize
      , Section () "txFeeFixed"          $ toValue protocolParamTxFeeFixed
      , Section () "txFeePerByte"        $ toValue protocolParamTxFeePerByte
      , Section () "minUTxOValue"        $ toValue protocolParamMinUTxOValue
      , Section () "stakePoolDeposit"    $ toValue protocolParamStakePoolDeposit
      , Section () "minPoolCost"         $ toValue protocolParamMinPoolCost
      , Section () "poolRetireMaxEpoch"  $ toValue protocolParamPoolRetireMaxEpoch
      , Section () "stakePoolTargetNum"  $ toValue protocolParamStakePoolTargetNum
      , Section () "poolPledgeInfluence" $ toValue protocolParamPoolPledgeInfluence
      , Section () "monetaryExpansion"   $ toValue protocolParamMonetaryExpansion
      , Section () "treasuryCut"         $ toValue protocolParamTreasuryCut
      , Section () "UTxOCostPerWord"     $ toValue protocolParamUTxOCostPerWord
      , Section () "costModels"          $ toValue protocolParamCostModels
      , Section () "prices"              $ toValue protocolParamPrices
      , Section () "maxTxExUnits"        $ toValue protocolParamMaxTxExUnits
      , Section () "maxBlockExUnits"     $ toValue protocolParamMaxBlockExUnits
      , Section () "maxValueSize"        $ toValue protocolParamMaxValueSize
      , Section () "collateralPercent"   $ toValue protocolParamCollateralPercent
      , Section () "maxCollateralInputs" $ toValue protocolParamMaxCollateralInputs
      ]
{- ORMOLU_ENABLE -}

maybeSpec :: ValueSpec a -> ValueSpec (Maybe a)
maybeSpec spec =
  Nothing <$ atomSpec "nothing"
    <!> Just <$> spec

protocolParamsSpec :: ValueSpec ProtocolParameters
protocolParamsSpec = sectionsSpec "ProtocolParameters" $ do
  protocolParamProtocolVersion <-
    optSectionFromDef'
      protocolParamProtocolVersion
      "protocolVersion"
      protocolVersionSpec
      "Protocol version, major and minor. Updating the major version is used to trigger hard forks."

  protocolParamDecentralization <-
    optSectionFromDef'
      protocolParamDecentralization
      "decentralization"
      customRationalSpec
      "The decentralization parameter. This is fraction of slots that belong to the BFT overlay schedule, rather than the Praos schedule. So 1 means fully centralised, while 0 means fully decentralised."

  protocolParamExtraPraosEntropy <-
    optSectionFromDef'
      protocolParamExtraPraosEntropy
      "extraPraosEntropy"
      (maybeSpec (makePraosNonce . fromString <$> stringSpec))
      "Extra entropy for the Praos per-epoch nonce."

  protocolParamMaxBlockHeaderSize <-
    optSectionFromDef
      protocolParamMaxBlockHeaderSize
      "maxBlockHeaderSize"
      "The maximum permitted size of a block header."

  protocolParamMaxBlockBodySize <-
    optSectionFromDef
      protocolParamMaxBlockBodySize
      "maxBlockBodySize"
      "The maximum permitted size of the block body (that is, the block payload, without the block header)."

  protocolParamMaxTxSize <-
    optSectionFromDef
      protocolParamMaxTxSize
      "maxTxSize"
      "The maximum permitted size of the block body (that is, the block payload, without the block header)."

  protocolParamTxFeeFixed <-
    optSectionFromDef
      protocolParamTxFeeFixed
      "txFeeFixed"
      "The constant factor for the minimum fee calculation."

  protocolParamTxFeePerByte <-
    optSectionFromDef
      protocolParamTxFeePerByte
      "txFeePerByte"
      "The linear factor for the minimum fee calculation."

  protocolParamMinUTxOValue <-
    optSectionFromDef'
      protocolParamMinUTxOValue
      "minUTxOValue"
      (maybeSpec lovelaceSpec)
      "The minimum permitted value for new UTxO entries, ie for transaction outputs."

  protocolParamStakePoolDeposit <-
    optSectionFromDef'
      protocolParamStakePoolDeposit
      "stakePoolDeposit"
      lovelaceSpec
      "The deposit required to register a stake pool."

  protocolParamStakeAddressDeposit <-
    optSectionFromDef
      protocolParamStakeAddressDeposit
      "stakeAddressDeposit"
      "The deposit required to register a stake address."

  protocolParamMinPoolCost <-
    optSectionFromDef
      protocolParamMinPoolCost
      "minPoolCost"
      "The minimum value that stake pools are permitted to declare for their cost parameter."

  protocolParamPoolRetireMaxEpoch <-
    optSectionFromDef'
      protocolParamPoolRetireMaxEpoch
      "poolRetireMaxEpoch"
      (EpochNo <$> anySpec)
      "The maximum number of epochs into the future that stake pools are permitted to schedule a retirement."

  protocolParamStakePoolTargetNum <-
    optSectionFromDef
      protocolParamStakePoolTargetNum
      "stakePoolTargetNum"
      "The equilibrium target number of stake pools."

  protocolParamPoolPledgeInfluence <-
    optSectionFromDef'
      protocolParamPoolPledgeInfluence
      "poolPledgeInfluence"
      customRationalSpec
      "The influence of the pledge in stake pool rewards."

  protocolParamMonetaryExpansion <-
    optSectionFromDef'
      protocolParamMonetaryExpansion
      "monetaryExpansion"
      customRationalSpec
      "The monetary expansion rate. This determines the fraction of the reserves that are added to the fee pot each epoch."

  protocolParamTreasuryCut <-
    optSectionFromDef'
      protocolParamTreasuryCut
      "treasuryCut"
      customRationalSpec
      "The fraction of the fee pot each epoch that goes to the treasury."

  protocolParamUTxOCostPerWord <-
    optSectionFromDef'
      protocolParamUTxOCostPerWord
      "UTxOCostPerWord"
      (maybeSpec lovelaceSpec)
      "Cost in ada per word of UTxO storage."

  protocolParamCostModels <-
    optSectionFromDef'
      protocolParamCostModels
      "costModels"
      versionCostModelSpec
      "Cost models for script languages that use them."

  protocolParamPrices <-
    optSectionFromDef'
      protocolParamPrices
      "prices"
      (maybeSpec executionUnitPricesSpec)
      "Price of execution units for script languages that use them."

  protocolParamMaxTxExUnits <-
    optSectionFromDef'
      protocolParamMaxTxExUnits
      "maxTxExUnits"
      (maybeSpec executionUnitsSpec)
      "Max total script execution resources units allowed per tx."

  protocolParamMaxBlockExUnits <-
    optSectionFromDef'
      protocolParamMaxBlockExUnits
      "maxBlockExUnits"
      (maybeSpec executionUnitsSpec)
      "Max size of a Value in a tx output."

  protocolParamMaxValueSize <-
    optSectionFromDef'
      protocolParamMaxValueSize
      "maxValueSize"
      (maybeSpec naturalSpec)
      "Max size of a Value in a tx output."

  protocolParamCollateralPercent <-
    optSectionFromDef'
      protocolParamCollateralPercent
      "collateralPercent"
      (maybeSpec naturalSpec)
      "The percentage of the script contribution to the txfee that must be provided as collateral inputs when including Plutus scripts."

  protocolParamMaxCollateralInputs <-
    optSectionFromDef'
      protocolParamMaxCollateralInputs
      "maxCollateralInputs"
      (maybeSpec naturalSpec)
      "The maximum number of collateral inputs allowed in a transaction."

  pure ProtocolParameters {..}

---------------

slotConfigSpec :: ValueSpec SlotConfig
slotConfigSpec = sectionsSpec "slotConfig - configure the length (ms) of one slot and the beginning of the first slot." $ do
  scSlotLength <- reqSection' "slotLength" integerSpec "Length (number of milliseconds) of one slot"
  scSlotZeroTime <- reqSection' "slotZeroTime" (POSIXTime <$> integerSpec) "Beginning of slot 0 (in milliseconds)"
  pure SlotConfig {..}

baseUrlSpec :: ValueSpec BaseUrl
baseUrlSpec =
  customSpec
    "URL"
    anySpec
    ( \x -> case parseBaseUrl $ Text.unpack x of
        Left e -> Left $ Text.pack $ show e
        Right url -> Right url
    )

pubKeyHashSpec :: ValueSpec PubKeyHash
pubKeyHashSpec = PubKeyHash . stringToBuiltinByteString <$> stringSpec

pabConfigSpec :: ValueSpec PABConfig
pabConfigSpec = sectionsSpec "PABConfig" $ do
  pcCliLocation <- optSectionFromDef' pcCliLocation "cliLocation" cliLocationSpec "Calling the cli through ssh"
  pcChainIndexUrl <-
    let def_ = pcChainIndexUrl def
        desc = Text.concat ["chain index URL (default: ", Text.pack $ showBaseUrl def_, ")"]
     in fromMaybe def_ <$> optSection' "baseUrlSpec" baseUrlSpec desc
  pcNetwork <- optSectionFromDef' pcNetwork "networkId" networkIdSpec ""
  pcProtocolParams <- optSectionFromDef' pcProtocolParams "protocolParams" protocolParamsSpec ""

  pcSlotConfig <- optSectionFromDef' pcSlotConfig "slotConfig" slotConfigSpec "slot config"

  pcScriptFileDir <- optSectionFromDef pcScriptFileDir "scriptFileDir" "Directory name of the script and data files"
  pcSigningKeyFileDir <-
    optSectionFromDef pcSigningKeyFileDir "signingKeyFileDir" "Directory name of the signing key files"
  pcTxFileDir <-
    optSectionFromDef pcTxFileDir "txFileDir" "Directory name of the transaction files"
  pcProtocolParamsFile <-
    optSectionFromDef pcProtocolParamsFile "protocolParamsFile" "Protocol params file location relative to the cardano-cli working directory (needed for the cli)"
  pcDryRun <-
    optSectionFromDef' pcDryRun "dryRun" trueOrFalseSpec "Dry run mode will build the tx, but skip the submit step"
  pcLogLevel <-
    optSectionFromDef' pcLogLevel "logLevel" logLevelSpec ""
  pcOwnPubKeyHash <-
    optSectionFromDef' pcOwnPubKeyHash "ownPubKeyHash" pubKeyHashSpec ""
  pcTipPollingInterval <-
    optSectionFromDef' pcTipPollingInterval "tipPollingInterval" naturalSpec ""
  pcPort <-
    optSectionFromDef' pcPort "port" (fromEnum <$> naturalSpec) ""
  pcEnableTxEndpoint <-
    optSectionFromDef' pcEnableTxEndpoint "enableTxEndpoint" trueOrFalseSpec ""
  pure PABConfig {..}

optSectionWithDef :: (Show a, HasSpec a) => a -> Text -> Text -> SectionsSpec a
optSectionWithDef def_ section desc =
  optSectionWithDef' def_ section anySpec desc

optSectionWithDef' :: (Show b) => b -> Text -> ValueSpec b -> Text -> SectionsSpec b
optSectionWithDef' def_ section spec desc =
  let defDesc = "(default: " <> Text.pack (show def_) <> ")"
      desc' = desc <> if Text.null desc then "" else " " <> defDesc
   in fromMaybe def_ <$> optSection' section spec desc'

optSectionFromDef :: (Default a, Show b, HasSpec b) => (a -> b) -> Text -> Text -> SectionsSpec b
optSectionFromDef getter section desc =
  optSectionWithDef' (getter def) section anySpec desc

optSectionFromDef' :: (Default a, Show b) => (a -> b) -> Text -> ValueSpec b -> Text -> SectionsSpec b
optSectionFromDef' getter section spec desc =
  optSectionWithDef' (getter def) section spec desc

configDoc :: String
configDoc = show $ generateDocs pabConfigSpec

loadPABConfig :: FilePath -> IO PABConfig
loadPABConfig = loadValueFromFile pabConfigSpec

showConf :: (ToValue a) => a -> String
showConf c = render $ pretty $ toValue c

readValue text = either (error . displayException) pretty $ parse $ Text.pack text

readConf spec text = either (error . displayException) (loadValue spec) $ parse $ Text.pack text

pDef = def :: ProtocolParameters
cDef = def :: PABConfig
