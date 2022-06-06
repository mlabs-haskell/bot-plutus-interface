{-# LANGUAGE NamedFieldPuns #-}

module Spec.BotPlutusInterface.Config (tests) where

import BotPlutusInterface.Config (loadPABConfig, savePABConfig)
import BotPlutusInterface.Types (CLILocation (..), LogLevel (..), PABConfig (..))
import Cardano.Api (
  AnyPlutusScriptVersion (..),
  CostModel (..),
  EpochNo (EpochNo),
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  Lovelace (..),
  NetworkId (Mainnet),
  PlutusScriptVersion (PlutusScriptV2),
  makePraosNonce,
 )
import Cardano.Api.Shelley (ProtocolParameters (..))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as Text
import Ledger (StakePubKeyHash (StakePubKeyHash))
import PlutusConfig.Misc (serializeDeserialize)
import Servant.Client.Core (BaseUrl (..), Scheme (Https))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

{- | Tests for PABConfig serialize/deserialize

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.Config"
    [ testCase
        "PABConfig default serialize/deserialize"
        (serializeDeserialize def @?= Right (def :: PABConfig))
    , testCase
        "PABConfig example serialize/deserialize (without pcProtocolParams)"
        ( serializeDeserialize pabConfigExample
            @?= Right pabConfigExample {pcProtocolParams = def}
        )
    , testCase
        "PABConfig example serialize/deserialize (without pcProtocolParams)"
        $ withSystemTempDirectory "PABConfig-test" $ \path -> do
          let confFile = path </> "conf.value"
              pcProtocolParamsFile = Text.pack $ path </> "protocol.json"
              conf = pabConfigExample {pcProtocolParamsFile}
          savePABConfig confFile conf
          Right conf' <- loadPABConfig confFile
          conf' @?= conf
    ]

pabConfigExample :: PABConfig
pabConfigExample =
  PABConfig
    { pcCliLocation = Remote "localhost"
    , pcChainIndexUrl = BaseUrl Https "127.0.0.1" 8080 ""
    , pcNetwork = Mainnet
    , pcProtocolParams =
        ProtocolParameters
          { protocolParamProtocolVersion = (4, 1)
          , protocolParamDecentralization = 2 % 4
          , protocolParamExtraPraosEntropy = Just $ makePraosNonce "HASH2"
          , protocolParamMaxBlockHeaderSize = 1001
          , protocolParamMaxBlockBodySize = 1002
          , protocolParamMaxTxSize = 1003
          , protocolParamTxFeeFixed = 1004
          , protocolParamTxFeePerByte = 1005
          , protocolParamMinUTxOValue = Just 1006
          , protocolParamStakeAddressDeposit = Lovelace 1007
          , protocolParamStakePoolDeposit = Lovelace 1008
          , protocolParamMinPoolCost = Lovelace 1009
          , protocolParamPoolRetireMaxEpoch = EpochNo 19
          , protocolParamStakePoolTargetNum = 1010
          , protocolParamPoolPledgeInfluence = 2 % 8
          , protocolParamMonetaryExpansion = 4 % 1011
          , protocolParamTreasuryCut = 5 % 7
          , protocolParamUTxOCostPerWord = Nothing
          , protocolParamCostModels =
              Map.fromList
                [
                  ( AnyPlutusScriptVersion PlutusScriptV2
                  , CostModel
                      ( Map.fromList
                          [ ("add_integer-cpu-arguments-intercept", 123456)
                          ]
                      )
                  )
                ]
          , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 1 % 9, priceExecutionMemory = 1 % 10})
          , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 1012, executionMemory = 1013})
          , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 1014, executionMemory = 1015})
          , protocolParamMaxValueSize = Just 1016
          , protocolParamCollateralPercent = Just 1017
          , protocolParamMaxCollateralInputs = Just 1018
          }
    , pcTipPollingInterval = 1021
    , pcScriptFileDir = "./result-scripts2"
    , pcSigningKeyFileDir = "./signing-keys2"
    , pcTxFileDir = "./txs2"
    , pcMetadataDir = "path"
    , pcDryRun = False
    , pcProtocolParamsFile = "./protocol.json3"
    , pcLogLevel = Debug
    , pcOwnPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    , pcOwnStakePubKeyHash = Just $ StakePubKeyHash "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97547"
    , pcPort = 1021
    , pcEnableTxEndpoint = True
    , pcCollectStats = False
    }
