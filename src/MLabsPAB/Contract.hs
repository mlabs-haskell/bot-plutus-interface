{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module MLabsPAB.Contract (runContract) where

import Control.Lens ((^.))
import Control.Monad.Freer (Eff, LastMember, interpretM, runM, type (~>))
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Extras.Log (handleLogIgnore)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Writer (runWriter)
import Data.Aeson (Value)
import Data.Either.Combinators (fromRight)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Tx (Tx)
import Ledger.Tx qualified as Tx
import MLabsPAB.CardanoCLI qualified as CardanoCLI
import MLabsPAB.Files qualified as Files
import MLabsPAB.PreBalance qualified as PreBalance
import MLabsPAB.Types (ContractEnvironment (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (..))
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.ChainIndex.Types (TxStatus (..), TxValidity (..))
import Plutus.Contract.Checkpoint (Checkpoint (..))
import Plutus.Contract.Effects (
  BalanceTxResponse (..),
  ChainIndexQuery (..),
  ChainIndexResponse (..),
  PABReq (..),
  PABResp (..),
  WriteBalancedTxResponse (..),
 )
import Plutus.Contract.Resumable (Resumable (..))
import Plutus.Contract.Types (Contract (..), ContractEffs)
import Servant.Client (
  BaseUrl (..),
  ClientError (FailureResponse),
  ClientM,
  ResponseF (Response, responseStatusCode),
  Scheme (Http),
  mkClientEnv,
  runClientM,
 )
import System.Directory (createDirectoryIfMissing)
import Wallet.Emulator.Error (WalletAPIError (..))
import Wallet.Emulator.Types (Wallet)
import Prelude

runContract ::
  forall w s e a.
  (Monoid w) =>
  ContractEnvironment ->
  Wallet ->
  Contract w s e a ->
  IO (Either e a, w)
runContract contractEnv _ (Contract effs) = do
  runM $ handleContract contractEnv effs

handleContract ::
  forall w effs e a.
  (LastMember IO effs, Monoid w) =>
  ContractEnvironment ->
  Eff (ContractEffs w e) a ->
  Eff effs (Either e a, w)
handleContract contractEnv =
  handleResumable contractEnv
    . handleCheckpoint
    . runWriter
    . handleLogIgnore @Value
    . runError
    . raiseEnd

handleResumable ::
  forall effs.
  (LastMember IO effs) =>
  ContractEnvironment ->
  Eff (Resumable PABResp PABReq ': effs) ~> Eff effs
handleResumable contractEnv =
  interpretM
    ( \case
        RRequest o -> handlePABReq contractEnv o
        RSelect -> pure True
        RZero -> undefined
    )

-- | Mocking checkpoint calls
handleCheckpoint :: forall effs. (LastMember IO effs) => Eff (Checkpoint ': effs) ~> Eff effs
handleCheckpoint =
  interpretM
    ( \case
        DoCheckpoint -> pure ()
        AllocateKey -> pure 1
        Store {} -> pure ()
        Retrieve {} -> pure (Right Nothing)
    )

{- | Interpreting contract monad into CLI calls and chain index requests
 A few of these effects are not handled, these just return some dummy result to make the
 type system happy
-}
handlePABReq :: ContractEnvironment -> PABReq -> IO PABResp
handlePABReq contractEnv req = do
  print req
  resp <- case req of
    ----------------------
    -- Handled requests --
    ----------------------
    OwnPublicKeyReq ->
      -- TODO: Should be able to get this from the wallet, hardcoded for now
      pure $ OwnPublicKeyResp contractEnv.ceOwnPubKey
    OwnContractInstanceIdReq ->
      pure $ OwnContractInstanceIdResp (ceContractInstanceId contractEnv)
    ChainIndexQueryReq chainIndexQuery ->
      ChainIndexQueryResp <$> handleChainIndexReq contractEnv chainIndexQuery
    BalanceTxReq unbalancedTx ->
      BalanceTxResp <$> balanceTx contractEnv unbalancedTx
    WriteBalancedTxReq tx ->
      WriteBalancedTxResp <$> writeBalancedTx contractEnv tx
    ------------------------
    -- Unhandled requests --
    ------------------------
    AwaitSlotReq s -> pure $ AwaitSlotResp s
    AwaitTimeReq t -> pure $ AwaitTimeResp t
    -- AwaitUtxoSpentReq txOutRef -> pure $ AwaitUtxoSpentResp ChainIndexTx
    -- AwaitUtxoProducedReq Address -> pure $ AwaitUtxoProducedResp (NonEmpty ChainIndexTx)
    AwaitTxStatusChangeReq txId -> pure $ AwaitTxStatusChangeResp txId (Committed TxValid)
    -- CurrentSlotReq -> CurrentSlotResp Slot
    -- CurrentTimeReq -> CurrentTimeResp POSIXTime
    -- ExposeEndpointReq ActiveEndpoint -> ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    -- PosixTimeRangeToContainedSlotRangeReq POSIXTimeRange -> PosixTimeRangeToContainedSlotRangeResp (Either SlotConversionError SlotRange)
    _ -> pure $ OwnContractInstanceIdResp contractEnv.ceContractInstanceId

  print resp
  pure resp

-- | This is not identical to the real balancing, we only do a pre-balance at this stage
balanceTx :: ContractEnvironment -> UnbalancedTx -> IO BalanceTxResponse
balanceTx contractEnv UnbalancedTx {unBalancedTxTx, unBalancedTxUtxoIndex, unBalancedTxRequiredSignatories} = do
  -- TODO: getting own address from pub key
  let ownAddress = Ledger.pubKeyHashAddress $ Ledger.pubKeyHash contractEnv.ceOwnPubKey
  -- TODO: Handle paging
  -- (_, Page {pageItems}) <-
  --   queryChainIndex $
  --     ChainIndexClient.getUtxoAtAddress $
  --       addressCredential ownAddress
  -- chainIndexTxOuts <- traverse (queryChainIndexToMaybe . ChainIndexClient.getTxOut) pageItems
  -- let utxos =
  --       Map.fromList $
  --         catMaybes $ zipWith (\oref txout -> (,) <$> Just oref <*> txout) pageItems chainIndexTxOuts

  utxos <- CardanoCLI.utxosAt contractEnv.cePABConfig ownAddress
  privKeys <-
    fromRight (error "Reading signing key files failed")
      <$> Files.readPrivateKeys contractEnv.cePABConfig

  let utxoIndex = fmap Tx.toTxOut utxos <> unBalancedTxUtxoIndex
  print utxoIndex
  let eitherPreBalancedTx =
        PreBalance.preBalanceTx
          contractEnv.ceMinLovelaces
          contractEnv.ceFees
          utxoIndex
          ownAddress
          privKeys
          (Map.keys unBalancedTxRequiredSignatories)
          unBalancedTxTx

  case eitherPreBalancedTx of
    Left err -> pure $ BalanceTxFailed (InsufficientFunds err)
    Right tx -> pure $ BalanceTxSuccess tx

-- | This step would build tx files, write them to disk and submit them to the chain
writeBalancedTx :: ContractEnvironment -> Tx -> IO WriteBalancedTxResponse
writeBalancedTx contractEnv tx = do
  createDirectoryIfMissing False (Text.unpack contractEnv.cePABConfig.pcScriptFileDir)

  let (validatorScripts, redeemers, datums) =
        unzip3 $ mapMaybe Tx.inScripts $ Set.toList $ Tx.txInputs tx

      policyScripts = Set.toList $ Ledger.txMintScripts tx
      allDatums = datums <> Map.elems (Tx.txData tx)
      allRedeemers = redeemers <> Map.elems (Tx.txRedeemers tx)

  fileWriteRes <-
    Files.writeAll
      contractEnv.cePABConfig
      policyScripts
      validatorScripts
      allDatums
      allRedeemers

  case fileWriteRes of
    Left err ->
      pure $
        WriteBalancedTxFailed $
          OtherError $
            "Failed to write script file(s): " <> Text.pack (show err)
    Right _ -> do
      let requiredSigners = Map.keys $ tx ^. Tx.signatures
      CardanoCLI.buildTx contractEnv.cePABConfig contractEnv.ceOwnPubKey tx
      CardanoCLI.signTx contractEnv.cePABConfig requiredSigners

      result <-
        if contractEnv.cePABConfig.pcDryRun
          then pure Nothing
          else CardanoCLI.submitTx contractEnv.cePABConfig

      case result of
        Just err -> pure $ WriteBalancedTxFailed $ OtherError err
        Nothing -> pure $ WriteBalancedTxSuccess tx

handleChainIndexReq :: ContractEnvironment -> ChainIndexQuery -> IO ChainIndexResponse
handleChainIndexReq _ = \case
  DatumFromHash datumHash ->
    DatumHashResponse <$> queryChainIndexToMaybe (ChainIndexClient.getDatum datumHash)
  ValidatorFromHash validatorHash ->
    ValidatorHashResponse <$> queryChainIndexToMaybe (ChainIndexClient.getValidator validatorHash)
  MintingPolicyFromHash mintingPolicyHash ->
    MintingPolicyHashResponse
      <$> queryChainIndexToMaybe (ChainIndexClient.getMintingPolicy mintingPolicyHash)
  StakeValidatorFromHash stakeValidatorHash ->
    StakeValidatorHashResponse
      <$> queryChainIndexToMaybe (ChainIndexClient.getStakeValidator stakeValidatorHash)
  RedeemerFromHash _ ->
    pure $ RedeemerHashResponse Nothing
  -- RedeemerFromHash redeemerHash ->
  --   pure $ RedeemerHashResponse (Maybe Redeemer)
  TxOutFromRef txOutRef ->
    TxOutRefResponse <$> queryChainIndexToMaybe (ChainIndexClient.getTxOut txOutRef)
  TxFromTxId txId ->
    TxIdResponse <$> queryChainIndexToMaybe (ChainIndexClient.getTx txId)
  UtxoSetMembership txOutRef ->
    UtxoSetMembershipResponse <$> queryChainIndex (ChainIndexClient.getIsUtxo txOutRef)
  UtxoSetAtAddress credential -> do
    UtxoSetAtResponse <$> queryChainIndex (ChainIndexClient.getUtxoAtAddress credential)
  GetTip ->
    GetTipResponse <$> queryChainIndex ChainIndexClient.getTip

queryChainIndex :: ClientM a -> IO a
queryChainIndex endpoint = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM endpoint $ mkClientEnv manager' $ BaseUrl Http "localhost" 9083 ""
  pure $ either (error . show) id res

queryChainIndexToMaybe :: ClientM a -> IO (Maybe a)
queryChainIndexToMaybe endpoint = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM endpoint $ mkClientEnv manager' $ BaseUrl Http "localhost" 9083 ""
  case res of
    Right result -> pure $ Just result
    Left failureResp@(FailureResponse _ Response {responseStatusCode})
      | statusCode responseStatusCode == 404 -> pure Nothing
      | otherwise -> error (show failureResp)
    Left failureResp -> error (show failureResp)
