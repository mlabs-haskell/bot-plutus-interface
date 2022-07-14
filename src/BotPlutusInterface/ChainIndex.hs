{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.ChainIndex (handleChainIndexReq) where

import BotPlutusInterface.Types (PABConfig)
import Data.Kind (Type)
import Network.HTTP.Client (ManagerSettings (managerResponseTimeout), defaultManagerSettings, newManager, responseTimeoutNone)
import Network.HTTP.Types (Status (statusCode))
import Plutus.ChainIndex.Api (
  TxoAtAddressRequest (TxoAtAddressRequest),
  UtxoAtAddressRequest (UtxoAtAddressRequest),
  UtxoWithCurrencyRequest (UtxoWithCurrencyRequest),
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

handleChainIndexReq :: PABConfig -> ChainIndexQuery -> IO ChainIndexResponse
handleChainIndexReq pabConf = \case
  DatumFromHash datumHash ->
    DatumHashResponse <$> chainIndexQueryOne pabConf (ChainIndexClient.getDatum datumHash)
  ValidatorFromHash validatorHash ->
    ValidatorHashResponse <$> chainIndexQueryOne pabConf (ChainIndexClient.getValidator validatorHash)
  MintingPolicyFromHash mintingPolicyHash ->
    MintingPolicyHashResponse
      <$> chainIndexQueryOne pabConf (ChainIndexClient.getMintingPolicy mintingPolicyHash)
  StakeValidatorFromHash stakeValidatorHash ->
    StakeValidatorHashResponse
      <$> chainIndexQueryOne pabConf (ChainIndexClient.getStakeValidator stakeValidatorHash)
  RedeemerFromHash _ ->
    pure $ RedeemerHashResponse Nothing
  -- RedeemerFromHash redeemerHash ->
  --   pure $ RedeemerHashResponse (Maybe Redeemer)
  TxOutFromRef txOutRef ->
    TxOutRefResponse <$> chainIndexQueryOne pabConf (ChainIndexClient.getTxOut txOutRef)
  UnspentTxOutFromRef txOutRef ->
    UnspentTxOutResponse <$> chainIndexQueryOne pabConf (ChainIndexClient.getUnspentTxOut txOutRef)
  TxFromTxId txId ->
    TxIdResponse <$> chainIndexQueryOne pabConf (ChainIndexClient.getTx txId)
  UtxoSetMembership txOutRef ->
    UtxoSetMembershipResponse <$> chainIndexQueryMany pabConf (ChainIndexClient.getIsUtxo txOutRef)
  UtxoSetAtAddress page credential ->
    UtxoSetAtResponse
      <$> chainIndexQueryMany
        pabConf
        (ChainIndexClient.getUtxoSetAtAddress (UtxoAtAddressRequest (Just page) credential))
  UtxoSetWithCurrency page assetClass ->
    UtxoSetAtResponse
      <$> chainIndexQueryMany
        pabConf
        (ChainIndexClient.getUtxoSetWithCurrency (UtxoWithCurrencyRequest (Just page) assetClass))
  GetTip ->
    GetTipResponse <$> chainIndexQueryMany pabConf ChainIndexClient.getTip
  TxsFromTxIds txIds -> TxIdsResponse <$> chainIndexQueryMany pabConf (ChainIndexClient.getTxs txIds)
  TxoSetAtAddress page credential ->
    TxoSetAtResponse
      <$> chainIndexQueryMany
        pabConf
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
