{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module BotPlutusInterface.Config.Cardano.Api () where

import BotPlutusInterface.Config.Base
import BotPlutusInterface.Config.Types
import BotPlutusInterface.Types ()
import Cardano.Api (
  AnyPlutusScriptVersion (..),
  CostModel (..),
  EpochNo (EpochNo),
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  Lovelace (..),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (..),
  PraosNonce,
 )
import Config
import Config.Schema
import Data.Aeson (eitherDecode, encode)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String
import Data.String.ToString
import Data.Text qualified as Text
import Prelude

instance HasSpec PraosNonce where
  anySpec = textSpecViaJSON "PraosNonce"

instance ToValue PraosNonce where
  toValue = toValueTextViaJSON

instance ToValue EpochNo where
  toValue (EpochNo x) = Number () $ integerToNumber $ toInteger x

instance HasSpec EpochNo where
  anySpec = EpochNo <$> anySpec

instance ToValue NetworkId where
  toValue Mainnet = Atom () "mainnet"
  toValue (Testnet (NetworkMagic nid)) =
    Number () $ integerToNumber $ toInteger nid

instance HasSpec NetworkId where
  anySpec = networkIdSpec

networkIdSpec :: ValueSpec NetworkId
networkIdSpec =
  Mainnet <$ atomSpec "mainnet"
    <!> Testnet . NetworkMagic <$> anySpec

instance ToValue Lovelace where
  toValue (Lovelace x) = Number () $ integerToNumber x

instance HasSpec Lovelace where
  anySpec = lovelaceSpec

lovelaceSpec :: ValueSpec Lovelace
lovelaceSpec = Lovelace . toInteger <$> naturalSpec

instance ToValue ExecutionUnitPrices where
  toValue (ExecutionUnitPrices priceExecutionSteps priceExecutionMemory) =
    Sections
      ()
      [ Section () "steps" $ toValue priceExecutionSteps
      , Section () "memory" $ toValue priceExecutionMemory
      ]

instance HasSpec ExecutionUnitPrices where
  anySpec = executionUnitPricesSpec

executionUnitPricesSpec :: ValueSpec ExecutionUnitPrices
executionUnitPricesSpec =
  sectionsSpec "ExecutionUnitPrices configuration" $ do
    priceExecutionSteps <-
      reqSection'
        "steps"
        customRationalSpec
        "The prices for ExecutionUnits as a fraction of a Lovelace."
    priceExecutionMemory <-
      reqSection'
        "memory"
        customRationalSpec
        "The prices for ExecutionUnits as a fraction of a Lovelace."
    pure ExecutionUnitPrices {..}

instance ToValue ExecutionUnits where
  toValue (ExecutionUnits executionSteps executionMemory) =
    Sections
      ()
      [ Section () "steps" $ toValue executionSteps
      , Section () "memory" $ toValue executionMemory
      ]

instance HasSpec ExecutionUnits where
  anySpec = executionUnitsSpec

executionUnitsSpec :: ValueSpec ExecutionUnits
executionUnitsSpec =
  sectionsSpec "ExecutionUnits configuration" $ do
    executionSteps <-
      reqSection
        "steps"
        "This corresponds roughly to the time to execute a script."
    executionMemory <-
      reqSection
        "memory"
        "This corresponds roughly to the peak memory used during script execution."
    pure ExecutionUnits {..}

instance ToValue (Map AnyPlutusScriptVersion CostModel) where
  toValue m = List () $ map pair $ Map.assocs m
    where
      pair (ver, cost) =
        Sections
          ()
          [ Section () "scriptVersion" $ toValue ver
          , Section () "costModel" $ toValue cost
          ]

instance HasSpec (Map AnyPlutusScriptVersion CostModel) where
  anySpec = versionCostModelSpec

versionCostModelSpec :: ValueSpec (Map AnyPlutusScriptVersion CostModel)
versionCostModelSpec = Map.fromList <$> listSpec pair
  where
    pair = sectionsSpec "CostModel configuration" $ do
      ver <- reqSection' "scriptVersion" anyPlutusScriptVersionSpec ""
      cost <- reqSection' "costModel" costModelSpec ""
      pure (ver, cost)

instance ToValue AnyPlutusScriptVersion where
  toValue = Text () . fromString . filter (/= '"') . toString . encode

instance HasSpec AnyPlutusScriptVersion where
  anySpec = anyPlutusScriptVersionSpec

anyPlutusScriptVersionSpec :: ValueSpec AnyPlutusScriptVersion
anyPlutusScriptVersionSpec =
  customSpec
    "AnyPlutusScriptVersion"
    stringSpec
    (first Text.pack . eitherDecode . fromString . show)

instance ToValue CostModel where
  toValue (CostModel m) =
    Sections () $
      map (\(k, i) -> Section () k (toValue i)) $ Map.assocs m

instance HasSpec CostModel where
  anySpec = costModelSpec

costModelSpec :: ValueSpec CostModel
costModelSpec = CostModel . Map.fromList <$> assocSpec integerSpec
