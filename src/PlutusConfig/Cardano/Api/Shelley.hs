{-# LANGUAGE ApplicativeDo #-}

-- {-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans  #-}

module PlutusConfig.Cardano.Api.Shelley (
  -- *Utils
  readProtocolParametersJSON,
  writeProtocolParametersJSON,
) where

import Cardano.Api.Shelley (ProtocolParameters (..))

-- import Config (Section (Section), Value (List, Sections))
-- import Config.Schema (
--   HasSpec (anySpec),
--   ValueSpec,
--   customSpec,
--   listSpec,
--   naturalSpec,
--   sectionsSpec,
--  )
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as LazyByteString

-- import Data.Default (def)
-- import Data.Text qualified as Text
-- import Numeric.Natural (Natural)
-- import PlutusConfig.Base (customRationalSpec, maybeSpec)
import PlutusConfig.Cardano.Api ()

-- import PlutusConfig.Types (
--   ToValue (toValue),
--   sectionWithDefault,
--   sectionWithDefault',
--  )
import Prelude

-- instance ToValue (Natural, Natural) where
--   toValue (a, b) = List () [toValue a, toValue b]

-- protocolVersionSpec :: ValueSpec (Natural, Natural)
-- protocolVersionSpec =
--   customSpec
--     ""
--     (listSpec naturalSpec)
--     ( \case
--         [major, minor] -> Right (major, minor)
--         _ -> Left $ Text.pack "[MAJOR, MINOR]"
--     )

-- {- ORMOLU_DISABLE -}
-- instance ToValue ProtocolParameters where
--   toValue
--     ( ProtocolParameters
--         protocolParamProtocolVersion
--         protocolParamDecentralization
--         protocolParamExtraPraosEntropy
--         protocolParamMaxBlockHeaderSize
--         protocolParamMaxBlockBodySize
--         protocolParamMaxTxSize
--         protocolParamTxFeeFixed
--         protocolParamTxFeePerByte
--         protocolParamMinUTxOValue
--         protocolParamStakeAddressDeposit
--         protocolParamStakePoolDeposit
--         protocolParamMinPoolCost
--         protocolParamPoolRetireMaxEpoch
--         protocolParamStakePoolTargetNum
--         protocolParamPoolPledgeInfluence
--         protocolParamMonetaryExpansion
--         protocolParamTreasuryCut
--         protocolParamUTxOCostPerWord
--         protocolParamCostModels
--         protocolParamPrices
--         protocolParamMaxTxExUnits
--         protocolParamMaxBlockExUnits
--         protocolParamMaxValueSize
--         protocolParamCollateralPercent
--         protocolParamMaxCollateralInputs
--       ) =
--       Sections
--         ()
--         [ Section () "protocolVersion"     $ toValue protocolParamProtocolVersion
--         , Section () "decentralization"    $ toValue protocolParamDecentralization
--         , Section () "extraPraosEntropy"   $ toValue protocolParamExtraPraosEntropy
--         , Section () "maxBlockHeaderSize"  $ toValue protocolParamMaxBlockHeaderSize
--         , Section () "maxBlockBodySize"    $ toValue protocolParamMaxBlockBodySize
--         , Section () "maxTxSize"           $ toValue protocolParamMaxTxSize
--         , Section () "txFeeFixed"          $ toValue protocolParamTxFeeFixed
--         , Section () "txFeePerByte"        $ toValue protocolParamTxFeePerByte
--         , Section () "minUTxOValue"        $ toValue protocolParamMinUTxOValue
--         , Section () "stakeAddressDeposit" $ toValue protocolParamStakeAddressDeposit
--         , Section () "stakePoolDeposit"    $ toValue protocolParamStakePoolDeposit
--         , Section () "minPoolCost"         $ toValue protocolParamMinPoolCost
--         , Section () "poolRetireMaxEpoch"  $ toValue protocolParamPoolRetireMaxEpoch
--         , Section () "stakePoolTargetNum"  $ toValue protocolParamStakePoolTargetNum
--         , Section () "poolPledgeInfluence" $ toValue protocolParamPoolPledgeInfluence
--         , Section () "monetaryExpansion"   $ toValue protocolParamMonetaryExpansion
--         , Section () "treasuryCut"         $ toValue protocolParamTreasuryCut
--         , Section () "UTxOCostPerWord"     $ toValue protocolParamUTxOCostPerWord
--         , Section () "costModels"          $ toValue protocolParamCostModels
--         , Section () "prices"              $ toValue protocolParamPrices
--         , Section () "maxTxExUnits"        $ toValue protocolParamMaxTxExUnits
--         , Section () "maxBlockExUnits"     $ toValue protocolParamMaxBlockExUnits
--         , Section () "maxValueSize"        $ toValue protocolParamMaxValueSize
--         , Section () "collateralPercent"   $ toValue protocolParamCollateralPercent
--         , Section () "maxCollateralInputs" $ toValue protocolParamMaxCollateralInputs
--         ]
-- {- ORMOLU_ENABLE -}

-- instance HasSpec ProtocolParameters where
--   anySpec = sectionsSpec "ProtocolParameters configuration" $ do
--     protocolParamProtocolVersion <-
--       sectionWithDefault'
--         (protocolParamProtocolVersion def)
--         "protocolVersion"
--         protocolVersionSpec
--         "Protocol version, major and minor. Updating the major version is used to trigger hard forks."

--     protocolParamDecentralization <-
--       sectionWithDefault'
--         (protocolParamDecentralization def)
--         "decentralization"
--         (fmap Just customRationalSpec)
--         "The decentralization parameter. This is fraction of slots that belong to the BFT overlay schedule, rather than the Praos schedule. So 1 means fully centralised, while 0 means fully decentralised."

--     protocolParamExtraPraosEntropy <-
--       sectionWithDefault'
--         (protocolParamExtraPraosEntropy def)
--         "extraPraosEntropy"
--         (maybeSpec anySpec)
--         "Extra entropy for the Praos per-epoch nonce."

--     protocolParamMaxBlockHeaderSize <-
--       sectionWithDefault
--         (protocolParamMaxBlockHeaderSize def)
--         "maxBlockHeaderSize"
--         "The maximum permitted size of a block header."

--     protocolParamMaxBlockBodySize <-
--       sectionWithDefault
--         (protocolParamMaxBlockBodySize def)
--         "maxBlockBodySize"
--         "The maximum permitted size of the block body (that is, the block payload, without the block header)."

--     protocolParamMaxTxSize <-
--       sectionWithDefault
--         (protocolParamMaxTxSize def)
--         "maxTxSize"
--         "The maximum permitted size of a transaction."

--     protocolParamTxFeeFixed <-
--       sectionWithDefault
--         (protocolParamTxFeeFixed def)
--         "txFeeFixed"
--         "The constant factor for the minimum fee calculation."

--     protocolParamTxFeePerByte <-
--       sectionWithDefault
--         (protocolParamTxFeePerByte def)
--         "txFeePerByte"
--         "The linear factor for the minimum fee calculation."

--     protocolParamMinUTxOValue <-
--       sectionWithDefault'
--         (protocolParamMinUTxOValue def)
--         "minUTxOValue"
--         (maybeSpec anySpec)
--         "The minimum permitted value for new UTxO entries, ie for transaction outputs."

--     protocolParamStakePoolDeposit <-
--       sectionWithDefault
--         (protocolParamStakePoolDeposit def)
--         "stakePoolDeposit"
--         "The deposit required to register a stake address."

--     protocolParamStakeAddressDeposit <-
--       sectionWithDefault
--         (protocolParamStakeAddressDeposit def)
--         "stakeAddressDeposit"
--         "The deposit required to register a stake pool."

--     protocolParamMinPoolCost <-
--       sectionWithDefault
--         (protocolParamMinPoolCost def)
--         "minPoolCost"
--         "The minimum value that stake pools are permitted to declare for their cost parameter."

--     protocolParamPoolRetireMaxEpoch <-
--       sectionWithDefault
--         (protocolParamPoolRetireMaxEpoch def)
--         "poolRetireMaxEpoch"
--         "The maximum number of epochs into the future that stake pools are permitted to schedule a retirement."

--     protocolParamStakePoolTargetNum <-
--       sectionWithDefault
--         (protocolParamStakePoolTargetNum def)
--         "stakePoolTargetNum"
--         "The equilibrium target number of stake pools."

--     protocolParamPoolPledgeInfluence <-
--       sectionWithDefault'
--         (protocolParamPoolPledgeInfluence def)
--         "poolPledgeInfluence"
--         customRationalSpec
--         "The influence of the pledge in stake pool rewards."

--     protocolParamMonetaryExpansion <-
--       sectionWithDefault'
--         (protocolParamMonetaryExpansion def)
--         "monetaryExpansion"
--         customRationalSpec
--         "The monetary expansion rate. This determines the fraction of the reserves that are added to the fee pot each epoch."

--     protocolParamTreasuryCut <-
--       sectionWithDefault'
--         (protocolParamTreasuryCut def)
--         "treasuryCut"
--         customRationalSpec
--         "The fraction of the fee pot each epoch that goes to the treasury."

--     protocolParamUTxOCostPerWord <-
--       sectionWithDefault'
--         (protocolParamUTxOCostPerWord def)
--         "UTxOCostPerWord"
--         (maybeSpec anySpec)
--         "Cost in ada per word of UTxO storage."

--     protocolParamCostModels <-
--       sectionWithDefault
--         (protocolParamCostModels def)
--         "costModels"
--         "Cost models for script languages that use them."

--     protocolParamPrices <-
--       sectionWithDefault'
--         (protocolParamPrices def)
--         "prices"
--         (maybeSpec anySpec)
--         "Price of execution units for script languages that use them."

--     protocolParamMaxTxExUnits <-
--       sectionWithDefault'
--         (protocolParamMaxTxExUnits def)
--         "maxTxExUnits"
--         (maybeSpec anySpec)
--         "Max total script execution resources units allowed per tx."

--     protocolParamMaxBlockExUnits <-
--       sectionWithDefault'
--         (protocolParamMaxBlockExUnits def)
--         "maxBlockExUnits"
--         (maybeSpec anySpec)
--         "Max total script execution resources units allowed per block"

--     protocolParamMaxValueSize <-
--       sectionWithDefault'
--         (protocolParamMaxValueSize def)
--         "maxValueSize"
--         (maybeSpec naturalSpec)
--         "Max size of a Value in a tx output."

--     protocolParamCollateralPercent <-
--       sectionWithDefault'
--         (protocolParamCollateralPercent def)
--         "collateralPercent"
--         (maybeSpec naturalSpec)
--         "The percentage of the script contribution to the txfee that must be provided as collateral inputs when including Plutus scripts."

--     protocolParamMaxCollateralInputs <-
--       sectionWithDefault'
--         (protocolParamMaxCollateralInputs def)
--         "maxCollateralInputs"
--         (maybeSpec naturalSpec)
--         "The maximum number of collateral inputs allowed in a transaction."

--     pure ProtocolParameters {..}

readProtocolParametersJSON :: FilePath -> IO (Either String ProtocolParameters)
readProtocolParametersJSON fn = (JSON.eitherDecode <$> LazyByteString.readFile fn) `catch` (\(e :: IOException) -> pure $ Left (show e))

writeProtocolParametersJSON :: FilePath -> ProtocolParameters -> IO ()
writeProtocolParametersJSON fn params =
  LazyByteString.writeFile fn $ JSON.encode params
