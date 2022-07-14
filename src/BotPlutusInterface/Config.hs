{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config (
  docPABConfig,
  loadPABConfig,
  savePABConfig,
) where

import BotPlutusInterface.Effects (
  ShellArgs (..),
  callLocalCommand,
 )
import BotPlutusInterface.Types (
  CLILocation (..),
  LogLevel (..),
  PABConfig (..),
  TxStatusPolling (TxStatusPolling, spBlocksTimeOut, spInterval),
 )

import Cardano.Api (NetworkId (Mainnet, Testnet), unNetworkMagic)
import Config (Section (Section), Value (Atom, Sections, Text))
import Config.Schema (
  HasSpec (anySpec),
  ValueSpec,
  atomSpec,
  generateDocs,
  naturalSpec,
  reqSection',
  sectionsSpec,
  trueOrFalseSpec,
  (<!>),
 )
import Data.Default (def)
import Data.String.ToString (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusConfig.Base (
  customRationalSpec,
  enumToAtom,
  filepathSpec,
  maybeSpec,
  pathSpec,
  portSpec,
 )
import PlutusConfig.Cardano.Api ()
import PlutusConfig.Cardano.Api.Shelley (readProtocolParametersJSON)
import PlutusConfig.Ledger ()
import PlutusConfig.Types (
  ToValue (toValue),
  deserialize,
  sectionWithDefault,
  sectionWithDefault',
  serialize,
  withNamePrefixSpec,
 )
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

instance ToValue TxStatusPolling where
  toValue (TxStatusPolling interval timeout) =
    Sections
      ()
      [ Section () "pollingInterval" $ toValue interval
      , Section () "pollingTimeout" $ toValue timeout
      ]

txStatusPollingSpec :: ValueSpec TxStatusPolling
txStatusPollingSpec =
  sectionsSpec "TxStatusPolling configuration" $ do
    spInterval <-
      reqSection'
        "microseconds"
        naturalSpec
        "Interval between chain-index queries for transactions status change detection"
    spBlocksTimeOut <-
      reqSection'
        "blocks"
        naturalSpec
        "Timeout (in blocks) after which awaiting of transaction status change will be cancelled and current Status returned"
    pure $ TxStatusPolling {..}

{- ORMOLU_DISABLE -}
instance ToValue PABConfig where
  toValue
    ( PABConfig
        pcCliLocation
        pcChainIndexUrl
        pcNetwork
        _pcProtocolParams
        pcScriptFileDir
        pcSigningKeyFileDir
        pcTxFileDir
        pcMetadataDir
        pcProtocolParamsFile
        pcDryRun
        pcLogLevel
        pcOwnPubKeyHash
        pcOwnStakePubKeyHash
        pcTipPollingInterval
        pcPort
        pcEnableTxEndpoint
        pcCollectStats
        pcCollectLogs
        pcBudgetMultiplier
        pcTxStatusPolling
      ) =
      Sections
        ()
        [ Section () "cliLocation"        $ toValue pcCliLocation
        , Section () "chainIndexUrl"      $ toValue pcChainIndexUrl
        , Section () "networkId"          $ toValue pcNetwork
        -- due to conflict, should be stored in pcProtocolParamsFile .json file
        -- , Section () "protocolParams"     $ toValue pcProtocolParams
        , Section () "scriptFileDir"      $ toValue pcScriptFileDir
        , Section () "signingKeyFileDir"  $ toValue pcSigningKeyFileDir
        , Section () "txFileDir"          $ toValue pcTxFileDir
        , Section () "metadataDir"        $ toValue pcMetadataDir
        , Section () "protocolParamsFile" $ toValue pcProtocolParamsFile
        , Section () "dryRun"             $ toValue pcDryRun
        , Section () "logLevel"           $ toValue pcLogLevel
        , Section () "ownPubKeyHash"      $ toValue pcOwnPubKeyHash
        , Section () "ownStakePubKeyHash" $ toValue pcOwnStakePubKeyHash
        , Section () "tipPollingInterval" $ toValue pcTipPollingInterval
        , Section () "port"               $ toValue pcPort
        , Section () "enableTxEndpoint"   $ toValue pcEnableTxEndpoint
        , Section () "collectStats"       $ toValue pcCollectStats
        , Section () "collectLogs"        $ toValue pcCollectLogs
        , Section () "budgetMultiplier"   $ toValue pcBudgetMultiplier
        , Section () "pcTxStatusPolling"  $ toValue pcTxStatusPolling
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

  -- due to conflict with pcProtocolParams, should got from
  -- pcProtocolParamsFile .json file
  -- pcProtocolParams <-
  --   sectionWithDefault (pcProtocolParams def) "protocolParams" ""
  let pcProtocolParams = Nothing

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

  pcMetadataDir <-
    sectionWithDefault' (pcMetadataDir def) "metadataDir" pathSpec "Directory name of metadata files"

  pcProtocolParamsFile <-
    sectionWithDefault'
      (pcProtocolParamsFile def)
      "protocolParamsFile"
      filepathSpec
      "Protocol params file location relative to the cardano-cli working directory (needed for the cli) in JSON format. "

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

  pcOwnStakePubKeyHash <-
    sectionWithDefault' (pcOwnStakePubKeyHash def) "ownStakePubKeyHash" (maybeSpec anySpec) ""

  pcTipPollingInterval <-
    sectionWithDefault' (pcTipPollingInterval def) "tipPollingInterval" naturalSpec ""

  pcPort <-
    sectionWithDefault' (pcPort def) "port" portSpec ""

  pcEnableTxEndpoint <-
    sectionWithDefault' (pcEnableTxEndpoint def) "enableTxEndpoint" trueOrFalseSpec ""

  pcCollectStats <-
    sectionWithDefault'
      (pcCollectStats def)
      "collectStats"
      trueOrFalseSpec
      "Save some stats during contract run (only transactions execution budgets supported atm)"

  pcCollectLogs <-
    sectionWithDefault'
      (pcCollectLogs def)
      "collectLogs"
      trueOrFalseSpec
      "Save logs from contract execution: pab request logs and contract logs"

  pcBudgetMultiplier <-
    sectionWithDefault'
      (pcBudgetMultiplier def)
      "budgetMultiplier"
      customRationalSpec
      "Multiplier on the budgets automatically calculated"

  pcTxStatusPolling <-
    sectionWithDefault'
      (pcTxStatusPolling def)
      "pcTxStatusPolling"
      txStatusPollingSpec
      "Set interval between `chain-index` queries and number of blocks to wait until timeout while await Transaction status to change"

  pure PABConfig {..}

docPABConfig :: String
docPABConfig = show $ generateDocs pabConfigSpec

{- |Load 'PABConfig' from the file:

NOTE:

- default value for: @pcProtocolParamsFile@ is "./protocol.json"

- if @pcProtocolParamsFile == "./protocol.json"@ and file don't exist, try to
  fetch in from @cardano-cli@ on the fly (only for local)

- in other cases -- fail
-}
loadPABConfig :: FilePath -> IO (Either String PABConfig)
loadPABConfig fn = do
  confE <- deserialize <$> readFile fn
  case confE of
    Left err -> return $ Left $ "PABConfig: " <> fn <> ": " <> err
    Right conf@PABConfig {pcProtocolParamsFile, pcNetwork, pcCliLocation} -> do
      pparamsE <- readProtocolParametersJSON (toString pcProtocolParamsFile)
      case pparamsE of
        Left err
          | pcProtocolParamsFile == "./protocol.json"
              && pcCliLocation == Local -> do
            let shellArgs =
                  ShellArgs
                    { cmdName = "cardano-cli"
                    , cmdArgs =
                        [ "query"
                        , "protocol-parameters"
                        , "--out-file"
                        , "./protocol.json"
                        ]
                          ++ networkArg pcNetwork
                    , cmdOutParser = id
                    }
            callLocalCommand shellArgs
              >>= \case
                Left errPParams -> return $ Left $ Text.unpack errPParams
                Right _ -> loadPABConfig fn
          | otherwise ->
            return $ pparamsError pcProtocolParamsFile err
        Right pparams -> return $ Right conf {pcProtocolParams = Just pparams}
  where
    pparamsError f e = Left $ "protocolParamsFile: " <> toString f <> ": " <> e

networkArg :: NetworkId -> [Text]
networkArg Mainnet = ["--mainnet"]
networkArg (Testnet magic) = ["--testnet-magic", Text.pack $ show $ unNetworkMagic magic]

{- |Save 'PABConfig'.

NOTE: The functions don't save @pcProtocolParams@ because don't expect that it can be changed.
-}
savePABConfig :: FilePath -> PABConfig -> IO ()
savePABConfig fn conf = writeFile fn $ serialize conf <> "\n"
