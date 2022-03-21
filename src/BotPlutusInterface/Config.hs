{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config (
  docPABConfig,
  loadPABConfig,
  savePABConfig,
) where

import BotPlutusInterface.Config.Base (enumToAtom, filepathSpec, pathSpec, portSpec)
import BotPlutusInterface.Config.Cardano.Api ()
import BotPlutusInterface.Config.Cardano.Api.Shelley ()
import BotPlutusInterface.Config.Ledger ()
import BotPlutusInterface.Config.Types (
  ToValue (toValue),
  deserialize,
  sectionWithDefault,
  sectionWithDefault',
  serialize,
  withNamePrefixSpec,
 )
import BotPlutusInterface.Types (CLILocation (..), LogLevel (..), PABConfig (..))
import Config (Section (Section), Value (Atom, Sections, Text))
import Config.Schema (
  HasSpec (anySpec),
  ValueSpec,
  atomSpec,
  generateDocs,
  naturalSpec,
  sectionsSpec,
  trueOrFalseSpec,
  (<!>),
 )
import Data.Default (def)
import Data.Text qualified as Text
import Prelude

instance ToValue CLILocation where
  toValue Local = Atom () "local"
  toValue (Remote url) = Text () url

cliLocationSpec :: ValueSpec CLILocation
cliLocationSpec =
  Local <$ atomSpec "local"
    <!> Remote <$> withNamePrefixSpec "destination" anySpec

instance ToValue LogLevel where
  toValue = enumToAtom

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
pabConfigSpec = sectionsSpec "PABConfig" $ do
  pcCliLocation <-
    sectionWithDefault'
      (pcCliLocation def)
      "cliLocation"
      cliLocationSpec
      "calling the cli through ssh when set to destination"

  pcChainIndexUrl <-
    sectionWithDefault (pcChainIndexUrl def) "chainIndexUrl" ""

  pcNetwork <-
    sectionWithDefault (pcNetwork def) "networkId" ""

  pcProtocolParams <-
    sectionWithDefault (pcProtocolParams def) "protocolParams" ""

  pcSlotConfig <-
    sectionWithDefault (pcSlotConfig def) "slotConfig" ""

  pcScriptFileDir <-
    sectionWithDefault'
      (pcScriptFileDir def)
      "scriptFileDir"
      pathSpec
      "Directory name of the script and data files"

  pcSigningKeyFileDir <-
    sectionWithDefault'
      (pcSigningKeyFileDir def)
      "signingKeyFileDir"
      pathSpec
      "Directory name of the signing key files"

  pcTxFileDir <-
    sectionWithDefault'
      (pcTxFileDir def)
      "txFileDir"
      pathSpec
      "Directory name of the transaction files"

  pcProtocolParamsFile <-
    sectionWithDefault'
      (pcProtocolParamsFile def)
      "protocolParamsFile"
      filepathSpec
      $ Text.concat
        [ "Protocol params file location relative to the cardano-cli working directory (needed for the cli) in JSON format. "
        , "BE AWARE: can overwrite the 'pcProtocolParams' section."
        ]

  pcDryRun <-
    sectionWithDefault'
      (pcDryRun def)
      "dryRun"
      trueOrFalseSpec
      "Dry run mode will build the tx, but skip the submit step"

  pcLogLevel <-
    sectionWithDefault' (pcLogLevel def) "logLevel" logLevelSpec ""

  pcOwnPubKeyHash <-
    sectionWithDefault (pcOwnPubKeyHash def) "ownPubKeyHash" ""

  pcTipPollingInterval <-
    sectionWithDefault' (pcTipPollingInterval def) "tipPollingInterval" naturalSpec ""

  pcPort <-
    sectionWithDefault' (pcPort def) "port" portSpec ""

  pcEnableTxEndpoint <-
    sectionWithDefault' (pcEnableTxEndpoint def) "enableTxEndpoint" trueOrFalseSpec ""

  pure PABConfig {..}

docPABConfig :: String
docPABConfig = show $ generateDocs pabConfigSpec

loadPABConfig :: FilePath -> IO (Either String PABConfig)
loadPABConfig fn = deserialize <$> readFile fn

savePABConfig :: FilePath -> PABConfig -> IO ()
savePABConfig fn conf = writeFile fn $ serialize conf
