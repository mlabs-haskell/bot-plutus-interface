{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config (
  docPABConfig,
  loadPABConfig,
  savePABConfig,
  customRationalSpec,
) where

import BotPlutusInterface.Config.Base (customRationalSpec, portSpec)
import BotPlutusInterface.Config.Cardano.Api ()
import BotPlutusInterface.Config.Cardano.Api.Shelley ()
import BotPlutusInterface.Config.Ledger ()
import BotPlutusInterface.Config.Types
import BotPlutusInterface.Types
import Config
import Config.Schema
import Data.Default
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text qualified as Text
import Servant.Client.Core (showBaseUrl)
import Prelude

instance ToValue CLILocation where
  toValue Local =
    Atom () "local"
  toValue (Remote url) = Text () url

cliLocationSpec :: ValueSpec CLILocation
cliLocationSpec =
  Local <$ atomSpec "local"
    <!> Remote <$> anySpec

instance ToValue LogLevel where
  toValue = Atom () . MkAtom . Text.toLower . Text.pack . show

logLevelSpec :: ValueSpec LogLevel
logLevelSpec =
  Error <$ atomSpec "error"
    <!> Warn <$ atomSpec "warn"
    <!> Notice <$ atomSpec "notice"
    <!> Info <$ atomSpec "info"
    <!> Debug <$ atomSpec "debug"

{- ORMOLU_DISABLE -}
instance ToValue PABConfig where
  toValue
    ( PABConfig
        pcCliLocation
        pcChainIndexUrl
        pcNetwork
        pcProtocolParams
        pcSlotConfig
        pcScriptFileDir
        pcSigningKeyFileDir
        pcTxFileDir
        pcProtocolParamsFile
        pcDryRun
        pcLogLevel
        pcOwnPubKeyHash
        pcTipPollingInterval
        pcPort
        pcEnableTxEndpoint
      ) =
      Sections
        ()
        [ Section () "cliLocation"        $ toValue pcCliLocation
        , Section () "chainIndexUrl"      $ toValue pcChainIndexUrl
        , Section () "networkId"          $ toValue pcNetwork
        , Section () "protocolParams"     $ toValue pcProtocolParams
        , Section () "slotConfig"         $ toValue pcSlotConfig
        , Section () "scriptFileDir"      $ toValue pcScriptFileDir
        , Section () "signingKeyFileDir"  $ toValue pcSigningKeyFileDir
        , Section () "txFileDir"          $ toValue pcTxFileDir
        , Section () "protocolParamsFile" $ toValue pcProtocolParamsFile
        , Section () "dryRun"             $ toValue pcDryRun
        , Section () "logLevel"           $ toValue pcLogLevel
        , Section () "ownPubKeyHash"      $ toValue pcOwnPubKeyHash
        , Section () "tipPollingInterval" $ toValue pcTipPollingInterval
        , Section () "port"               $ toValue pcPort
        , Section () "enableTxEndpoint"   $ toValue pcEnableTxEndpoint
        ]
{- ORMOLU_ENABLE -}

instance HasSpec PABConfig where
  anySpec = pabConfigSpec

pabConfigSpec :: ValueSpec PABConfig
pabConfigSpec = sectionsSpec "" $ do
  pcCliLocation <-
    optSectionFromDef'
      pcCliLocation
      "cliLocation"
      cliLocationSpec
      "Calling the cli through ssh"

  pcChainIndexUrl <-
    let def_ = pcChainIndexUrl def
        desc =
          Text.concat
            ["chain index URL (default: ", Text.pack $ showBaseUrl def_, ")"]
     in fromMaybe def_ <$> optSection "chainIndexUrl" desc

  pcNetwork <- optSectionFromDef pcNetwork "networkId" ""

  pcProtocolParams <-
    optSectionFromDef
      pcProtocolParams
      "protocolParams"
      ""

  pcSlotConfig <-
    optSectionFromDef
      pcSlotConfig
      "slotConfig"
      "slot config"

  pcScriptFileDir <-
    optSectionFromDef
      pcScriptFileDir
      "scriptFileDir"
      "Directory name of the script and data files"

  pcSigningKeyFileDir <-
    optSectionFromDef
      pcSigningKeyFileDir
      "signingKeyFileDir"
      "Directory name of the signing key files"

  pcTxFileDir <-
    optSectionFromDef
      pcTxFileDir
      "txFileDir"
      "Directory name of the transaction files"

  pcProtocolParamsFile <-
    optSectionFromDef
      pcProtocolParamsFile
      "protocolParamsFile"
      "Protocol params file location relative to the cardano-cli working directory (needed for the cli)"

  pcDryRun <-
    optSectionFromDef'
      pcDryRun
      "dryRun"
      trueOrFalseSpec
      "Dry run mode will build the tx, but skip the submit step"

  pcLogLevel <-
    optSectionFromDef' pcLogLevel "logLevel" logLevelSpec ""

  pcOwnPubKeyHash <-
    optSectionFromDef pcOwnPubKeyHash "ownPubKeyHash" ""

  pcTipPollingInterval <-
    optSectionFromDef' pcTipPollingInterval "tipPollingInterval" naturalSpec ""

  pcPort <-
    optSectionFromDef' pcPort "port" portSpec ""

  pcEnableTxEndpoint <-
    optSectionFromDef' pcEnableTxEndpoint "enableTxEndpoint" trueOrFalseSpec ""
  pure PABConfig {..}

docPABConfig :: String
docPABConfig = show $ generateDocs pabConfigSpec

loadPABConfig :: FilePath -> IO (Either String PABConfig)
loadPABConfig fn = deserialize <$> readFile fn

savePABConfig :: FilePath -> PABConfig -> IO ()
savePABConfig fn conf = writeFile fn $ serialize conf
