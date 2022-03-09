{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
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
 )
import Cardano.Api.Shelley (ProtocolParameters (..), makePraosNonce)
import Config.Schema
import Data.Aeson (eitherDecode)
import Data.Bifunctor (bimap)
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text qualified as Text
import Ledger.TimeSlot (SlotConfig (..))
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Servant.Client.Core (BaseUrl (..), parseBaseUrl, showBaseUrl)
import Wallet.API (PubKeyHash (PubKeyHash))
import Prelude

cliLocationSpec :: ValueSpec CLILocation
cliLocationSpec =
  Local <$ atomSpec "Local"
    <!> Remote <$> anySpec

networkIdSpec :: ValueSpec NetworkId
networkIdSpec =
  Mainnet <$ atomSpec "mainnet"
    <!> Testnet . NetworkMagic <$> anySpec

protocolVersionSpec :: ValueSpec (Natural, Natural)
protocolVersionSpec =
  customSpec
    "protocol version"
    (listSpec naturalSpec)
    ( \x -> case x of
        [major, minor] -> Right (major, minor)
        _ -> Left $ Text.pack "[MAJOR, MINOR]"
    )

instance HasSpec Lovelace where
  anySpec = lovelaceSpec

lovelaceSpec :: ValueSpec Lovelace
lovelaceSpec = Lovelace . toInteger <$> naturalSpec

executionUnitPricesSpec :: ValueSpec ExecutionUnitPrices
executionUnitPricesSpec = sectionsSpec "The prices for 'ExecutionUnits' as a fraction of a 'Lovelace'." $ do
  priceExecutionSteps <- reqSection "steps" ""
  priceExecutionMemory <- reqSection "memory" ""
  pure ExecutionUnitPrices {..}

executionUnitsSpec :: ValueSpec ExecutionUnits
executionUnitsSpec =
  sectionsSpec "The units for how long a script executes for and how much memory it uses." $ do
    executionSteps <- reqSection "steps" "This corresponds roughly to the time to execute a script."
    executionMemory <- reqSection "memory" "This corresponds roughly to the peak memory used during script execution."
    pure ExecutionUnits {..}

anyPlutusScriptVersionSpec :: ValueSpec AnyPlutusScriptVersion
anyPlutusScriptVersionSpec =
  customSpec
    "AnyPlutusScriptVersion"
    stringSpec
    (bimap Text.pack id . eitherDecode . fromString . show)

costModelSpec :: ValueSpec CostModel
costModelSpec = CostModel . Map.fromList <$> assocSpec integerSpec

versionCostModelSpec :: ValueSpec (Map AnyPlutusScriptVersion CostModel)
versionCostModelSpec = Map.fromList <$> listSpec pair
  where
    pair = sectionsSpec "" $ do
      ver <- reqSection' "scriptVersion" anyPlutusScriptVersionSpec ""
      costModel <- reqSection' "costModel" costModelSpec ""
      pure (ver, costModel)

protocolParamsSpec :: ValueSpec ProtocolParameters
protocolParamsSpec = sectionsSpec "ProtocolParameters" $ do
  protocolParamProtocolVersion <-
    optSectionFromDef'
      protocolParamProtocolVersion
      "protocolVersion"
      protocolVersionSpec
      "Protocol version, major and minor. Updating the major version is used to trigger hard forks."

  protocolParamDecentralization <-
    optSectionFromDef
      protocolParamDecentralization
      "paramDecentralization"
      "The decentralization parameter. This is fraction of slots that belong to the BFT overlay schedule, rather than the Praos schedule. So 1 means fully centralised, while 0 means fully decentralised."

  protocolParamExtraPraosEntropy <-
    optSectionFromDef'
      protocolParamExtraPraosEntropy
      "extraPraosEntropy"
      (Just . makePraosNonce . fromString <$> stringSpec)
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
      "maxTxSize"
      "The constant factor for the minimum fee calculation."

  protocolParamTxFeePerByte <-
    optSectionFromDef
      protocolParamTxFeePerByte
      "txFeePerByte"
      "The linear factor for the minimum fee calculation."

  protocolParamMinUTxOValue <-
    optSectionFromDef'
      protocolParamMinUTxOValue
      "txFeePerByte"
      (Just <$> lovelaceSpec)
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
    optSectionFromDef
      protocolParamPoolPledgeInfluence
      "poolPledgeInfluence"
      "The influence of the pledge in stake pool rewards."

  protocolParamMonetaryExpansion <-
    optSectionFromDef
      protocolParamMonetaryExpansion
      "monetaryExpansion"
      "The monetary expansion rate. This determines the fraction of the reserves that are added to the fee pot each epoch."

  protocolParamTreasuryCut <-
    optSectionFromDef
      protocolParamTreasuryCut
      "treasuryCut"
      "The fraction of the fee pot each epoch that goes to the treasury."

  protocolParamUTxOCostPerWord <-
    optSectionFromDef'
      protocolParamUTxOCostPerWord
      "UTxOCostPerWord"
      (Just <$> lovelaceSpec)
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
      "paramPrices"
      (Just <$> executionUnitPricesSpec)
      "Price of execution units for script languages that use them."

  protocolParamMaxTxExUnits <-
    optSectionFromDef'
      protocolParamMaxTxExUnits
      "maxBlockExUnits"
      (Just <$> executionUnitsSpec)
      "Max total script execution resources units allowed per tx."

  protocolParamMaxBlockExUnits <-
    optSectionFromDef'
      protocolParamMaxBlockExUnits
      "maxBlockExUnits"
      (Just <$> executionUnitsSpec)
      "Max size of a Value in a tx output."

  protocolParamMaxValueSize <-
    optSectionFromDef'
      protocolParamMaxValueSize
      "collateralPercent"
      (Just <$> naturalSpec)
      "Max size of a Value in a tx output."

  protocolParamCollateralPercent <-
    optSectionFromDef'
      protocolParamCollateralPercent
      "collateralPercent"
      (Just <$> naturalSpec)
      "The percentage of the script contribution to the txfee that must be provided as collateral inputs when including Plutus scripts."

  protocolParamMaxCollateralInputs <-
    optSectionFromDef'
      protocolParamMaxCollateralInputs
      "maxCollateralInputs"
      (Just <$> naturalSpec)
      "The maximum number of collateral inputs allowed in a transaction."

  pure ProtocolParameters {..}

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

logLevelSpec :: ValueSpec LogLevel
logLevelSpec =
  Error <$ atomSpec "Error"
    <!> Warn <$ atomSpec "Warn"
    <!> Notice <$ atomSpec "Notice"
    <!> Info <$ atomSpec "Info"
    <!> Debug <$ atomSpec "Debug"

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

optSectionWithDef def_ section desc =
  optSectionWithDef' def_ section anySpec desc

optSectionWithDef' def_ section spec desc =
  let defDesc = "(default: " <> Text.pack (show def_) <> ")"
      desc' = desc <> if Text.null desc then "" else " " <> defDesc
   in fromMaybe def_ <$> optSection' section spec desc'

optSectionFromDef getter section desc =
  optSectionWithDef' (getter def) section anySpec desc

optSectionFromDef' getter section spec desc =
  optSectionWithDef' (getter def) section spec desc

configDoc :: String
configDoc = show $ generateDocs pabConfigSpec

loadPABConfig :: FilePath -> IO PABConfig
loadPABConfig = loadValueFromFile pabConfigSpec
