module Spec.PlutusConfig.Cardano.Api.Shelley (tests) where

import Cardano.Api (
  AnyPlutusScriptVersion (..),
  CostModel (..),
  EpochNo (EpochNo),
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  Lovelace (..),
  PlutusScriptVersion (PlutusScriptV2),
  makePraosNonce,
 )
import Cardano.Api.Shelley (ProtocolParameters (..))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Ratio ((%))
import PlutusConfig.Cardano.Api ()
import PlutusConfig.Cardano.Api.Shelley ()
import PlutusConfig.Ledger ()
import Spec.PlutusConfig.Misc (serializeDeserialize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "ProtocolParameters"
    [ testCase
        "from-to def ProtocolParameters"
        (serializeDeserialize (def :: ProtocolParameters) @?= Right def)
    , testCase
        "from-to custom ProtocolParameters"
        (serializeDeserialize customParams @?= Right customParams)
    ]

customParams :: ProtocolParameters
customParams =
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
