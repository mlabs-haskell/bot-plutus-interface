{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.ChainIndex (
  handleChainIndexReq,
) where

import BotPlutusInterface.Collateral (removeCollateralFromPage)
import BotPlutusInterface.Types (
  ContractEnvironment (ContractEnvironment, cePABConfig),
  PABConfig,
  readCollateralUtxo,
 )
import Data.Kind (Type)
import Network.HTTP.Client (
  ManagerSettings (managerResponseTimeout),
  defaultManagerSettings,
  newManager,
  responseTimeoutNone,
 )
import Network.HTTP.Types (Status (statusCode))
import Plutus.ChainIndex.Api (
  QueryAtAddressRequest (QueryAtAddressRequest),
  TxoAtAddressRequest (TxoAtAddressRequest),
  TxosResponse (TxosResponse),
  UtxoAtAddressRequest (UtxoAtAddressRequest),
  UtxoWithCurrencyRequest (UtxoWithCurrencyRequest),
  UtxosResponse (UtxosResponse),
 )
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import Servant.Client (
  ClientError (FailureResponse),
  ClientM,
  ResponseF (Response, responseStatusCode),
  mkClientEnv,
  runClientM,
 )
import Prelude

handleChainIndexReq :: forall (w :: Type). ContractEnvironment w -> ChainIndexQuery -> IO ChainIndexResponse
handleChainIndexReq contractEnv@ContractEnvironment {cePABConfig} =
  \case
    DatumFromHash datumHash ->
      DatumHashResponse <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getDatum datumHash)
    ValidatorFromHash validatorHash ->
      ValidatorHashResponse <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getValidator validatorHash)
    MintingPolicyFromHash mintingPolicyHash ->
      MintingPolicyHashResponse
        <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getMintingPolicy mintingPolicyHash)
    StakeValidatorFromHash stakeValidatorHash ->
      StakeValidatorHashResponse
        <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getStakeValidator stakeValidatorHash)
    RedeemerFromHash _ ->
      pure $ RedeemerHashResponse Nothing
    -- RedeemerFromHash redeemerHash ->
    --   pure $ RedeemerHashResponse (Maybe Redeemer)
    TxOutFromRef txOutRef ->
      TxOutRefResponse <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getTxOut txOutRef)
    UnspentTxOutFromRef txOutRef ->
      UnspentTxOutResponse <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getUnspentTxOut txOutRef)
    UnspentTxOutSetAtAddress page credential ->
      UnspentTxOutsAtResponse
        <$> chainIndexQueryMany
          cePABConfig
          (ChainIndexClient.getUnspentTxOutsAtAddress (QueryAtAddressRequest (Just page) credential))
    TxFromTxId txId ->
      TxIdResponse <$> chainIndexQueryOne cePABConfig (ChainIndexClient.getTx txId)
    UtxoSetMembership txOutRef ->
      UtxoSetMembershipResponse <$> chainIndexQueryMany cePABConfig (ChainIndexClient.getIsUtxo txOutRef)
    UtxoSetAtAddress page credential ->
      UtxoSetAtResponse
        <$> chainIndexUtxoQuery
          contractEnv
          (ChainIndexClient.getUtxoSetAtAddress (UtxoAtAddressRequest (Just page) credential))
    UtxoSetWithCurrency page assetClass ->
      UtxoSetAtResponse
        <$> chainIndexUtxoQuery
          contractEnv
          (ChainIndexClient.getUtxoSetWithCurrency (UtxoWithCurrencyRequest (Just page) assetClass))
    GetTip ->
      GetTipResponse <$> chainIndexQueryMany cePABConfig ChainIndexClient.getTip
    TxsFromTxIds txIds -> TxIdsResponse <$> chainIndexQueryMany cePABConfig (ChainIndexClient.getTxs txIds)
    TxoSetAtAddress page credential ->
      TxoSetAtResponse
        <$> chainIndexTxoQuery
          contractEnv
          (ChainIndexClient.getTxoSetAtAddress (TxoAtAddressRequest (Just page) credential))

chainIndexQuery' :: forall (a :: Type). PABConfig -> ClientM a -> IO (Either ClientError a)
chainIndexQuery' pabConf endpoint = do
  manager' <- newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
  runClientM endpoint $ mkClientEnv manager' pabConf.pcChainIndexUrl

chainIndexQueryMany :: forall (a :: Type). PABConfig -> ClientM a -> IO a
chainIndexQueryMany pabConf endpoint =
  either (error . show) id <$> chainIndexQuery' pabConf endpoint

chainIndexQueryOne :: forall (a :: Type). PABConfig -> ClientM a -> IO (Maybe a)
chainIndexQueryOne pabConf endpoint = do
  res <- chainIndexQuery' pabConf endpoint
  case res of
    Right result -> pure $ Just result
    Left failureResp@(FailureResponse _ Response {responseStatusCode})
      | statusCode responseStatusCode == 404 -> pure Nothing
      | otherwise -> error (show failureResp)
    Left failureResp -> error (show failureResp)

-- | Query for utxo's and filter collateral utxo from result.
chainIndexUtxoQuery :: forall (w :: Type). ContractEnvironment w -> ClientM UtxosResponse -> IO UtxosResponse
chainIndexUtxoQuery contractEnv query = do
  collateralUtxo <- readCollateralUtxo contractEnv
  let removeCollateral :: UtxosResponse -> UtxosResponse
      removeCollateral (UtxosResponse tip page) = UtxosResponse tip (removeCollateralFromPage collateralUtxo page)
  removeCollateral
    <$> chainIndexQueryMany
      contractEnv.cePABConfig
      query

-- | Query for txo's and filter collateral txo from result.
chainIndexTxoQuery :: forall (w :: Type). ContractEnvironment w -> ClientM TxosResponse -> IO TxosResponse
chainIndexTxoQuery contractEnv query = do
  collateralUtxo <- readCollateralUtxo contractEnv
  let removeCollateral :: TxosResponse -> TxosResponse
      removeCollateral (TxosResponse page) = TxosResponse (removeCollateralFromPage collateralUtxo page)
  removeCollateral
    <$> chainIndexQueryMany
      contractEnv.cePABConfig
      query
