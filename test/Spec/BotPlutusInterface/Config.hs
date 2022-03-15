module Spec.BotPlutusInterface.Config (tests) where

import BotPlutusInterface.Config
import BotPlutusInterface.Config.Types
import BotPlutusInterface.Types (CLILocation (..), LogLevel (..), PABConfig (..))
import Cardano.Api.Shelley
import Config.Schema
import Data.Default (def)
import Data.Either (isLeft)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Ledger.TimeSlot (SlotConfig (..))
import Plutus.V1.Ledger.Api (POSIXTime (..))
import Servant.Client.Core (BaseUrl (..), Scheme (Https))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Wallet.API (PubKeyHash (..))
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
        "PABConfig example serialize/deserialize"
        (serializeDeserialize pabConfigExample @?= Right pabConfigExample)
    , testPubKeyHash
    , testPraosNonce
    , testRationalSerializeDeserialize
    ]

testRationalSerializeDeserialize :: TestTree
testRationalSerializeDeserialize =
  testGroup
    "Rational"
    [ testCase
        "Rational serialize"
        (serialize (1 % 2 :: Rational) @?= "\"1 % 2\"")
    , testCase
        "Rational deserialize with value error"
        ( isLeft (deserialize' customRationalSpec "\"1 % 0\"")
            @? "should be Left"
        )
    , testCase
        "Rational deserialize with value error"
        ( isLeft (deserialize' customRationalSpec "\"1\"")
            @? "should be Left"
        )
    , testCase
        "Rational serialize/deserialize"
        ( serializeDeserialize' customRationalSpec (1 % 2)
            @?= Right (1 % 2 :: Rational)
        )
    ]

testPubKeyHash :: TestTree
testPubKeyHash =
  testGroup
    "PubKeyHash serialize/deserialize"
    [ testCase
        "PubKeyHash serialize"
        (serialize hash @?= shash)
    , testCase
        "PubKeyHash deserialize"
        (deserialize shash @?= Right hash)
    ]
  where
    hash = PubKeyHash "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    shash = "\"0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546\""

testPraosNonce :: TestTree
testPraosNonce =
  testGroup
    "PraosNonce serialize/deserialize"
    [ testCase
        "PraosNonce serialize"
        (serialize praosNonce @?= sPraosNonce)
    , testCase
        "PraosNonce deserialize"
        (deserialize sPraosNonce @?= Right praosNonce)
    ]
  where
    praosNonce = makePraosNonce "Some string"
    sPraosNonce = "\"33b058e9898a60f0fe95d41b92bcbff152a008785ff1b390fd594ff4fe282770\""

serializeDeserialize :: (ToValue a, HasSpec a) => a -> Either String a
serializeDeserialize = serializeDeserialize' anySpec

serializeDeserialize' :: (ToValue a) => ValueSpec a -> a -> Either String a
serializeDeserialize' spec = deserialize' spec . serialize

pabConfigExample :: PABConfig
pabConfigExample =
  def{ pcCliLocation = Remote "localhost"
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
     , pcSlotConfig = SlotConfig {scSlotLength = 1019, scSlotZeroTime = POSIXTime 1020}
     , pcTipPollingInterval = 1021
     , pcScriptFileDir = "./result-scripts2"
     , pcSigningKeyFileDir = "./signing-keys2"
     , pcTxFileDir = "./txs2"
     , pcDryRun = False
     , pcProtocolParamsFile = "./protocol.json3"
     , pcLogLevel = Debug
     , pcOwnPubKeyHash = PubKeyHash "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
     , pcPort = 1021
     , pcEnableTxEndpoint = True
     }
