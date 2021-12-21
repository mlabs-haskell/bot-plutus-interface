{-# LANGUAGE RankNTypes #-}

module MLabsPAB.Contract (runContract, handleContract) where

import Control.Lens ((^.))
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, runM, subsume, type (~>))
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Extras.Log (handleLogIgnore)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Writer (Writer (Tell))
import Data.Aeson (ToJSON, Value, toJSON)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Row (Row)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Tx (CardanoTx)
import Ledger.Tx qualified as Tx
import MLabsPAB.CardanoCLI qualified as CardanoCLI
import MLabsPAB.Effects (
  PABEffect,
  createDirectoryIfMissing,
  handlePABEffect,
  printLog,
  queryChainIndex,
  updateInstanceState,
 )
import MLabsPAB.Files qualified as Files
import MLabsPAB.PreBalance qualified as PreBalance
import MLabsPAB.Types (ContractEnvironment (..), LogLevel (Debug))
import Plutus.ChainIndex.Types (RollbackState (Committed), TxValidity (..))
import Plutus.Contract.Checkpoint (Checkpoint (..))
import Plutus.Contract.Effects (
  BalanceTxResponse (..),
  PABReq (..),
  PABResp (..),
  WriteBalancedTxResponse (..),
 )
import Plutus.Contract.Resumable (Resumable (..))
import Plutus.Contract.Types (Contract (..), ContractEffs)
import Plutus.PAB.Webserver.Types (InstanceStatusToClient (NewObservableState))
import Wallet.Emulator.Error (WalletAPIError (..))
import Wallet.Emulator.Types (Wallet)
import Prelude

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (ToJSON w) =>
  ContractEnvironment ->
  Wallet ->
  Contract w s e a ->
  IO (Either e a)
runContract contractEnv _ (Contract effs) = do
  runM $ handlePABEffect contractEnv $ raiseEnd $ handleContract contractEnv effs

handleContract ::
  forall (w :: Type) (e :: Type) (a :: Type).
  ToJSON w =>
  ContractEnvironment ->
  Eff (ContractEffs w e) a ->
  Eff '[PABEffect] (Either e a)
handleContract contractEnv =
  subsume
    . handleResumable contractEnv
    . handleCheckpointIgnore
    . handleWriter
    . handleLogIgnore @Value
    . runError
    . raiseEnd

handleWriter ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ToJSON w =>
  (Member PABEffect effs) =>
  Eff (Writer w ': effs)
    ~> Eff effs
handleWriter =
  interpret
    (\case Tell msg -> updateInstanceState (NewObservableState (toJSON msg)))

handleResumable ::
  forall (effs :: [Type -> Type]).
  ContractEnvironment ->
  Eff (Resumable PABResp PABReq ': effs) ~> Eff (PABEffect ': effs)
handleResumable contractEnv =
  reinterpret
    ( \case
        RRequest o -> handlePABReq contractEnv o
        RSelect -> pure True
        RZero -> undefined
    )

-- | Mocking checkpoint calls
handleCheckpointIgnore :: forall (effs :: [Type -> Type]). Eff (Checkpoint ': effs) ~> Eff effs
handleCheckpointIgnore =
  interpret
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
handlePABReq ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  ContractEnvironment ->
  PABReq ->
  Eff effs PABResp
handlePABReq contractEnv req = do
  printLog Debug $ show req
  resp <- case req of
    ----------------------
    -- Handled requests --
    ----------------------
    OwnPublicKeyHashReq ->
      -- TODO: Should be able to get this from the wallet, hardcoded for now
      pure $ OwnPublicKeyHashResp $ Ledger.pubKeyHash contractEnv.ceOwnPubKey
    OwnContractInstanceIdReq ->
      pure $ OwnContractInstanceIdResp (ceContractInstanceId contractEnv)
    ChainIndexQueryReq query ->
      ChainIndexQueryResp <$> queryChainIndex query
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
    AwaitTxStatusChangeReq txId -> pure $ AwaitTxStatusChangeResp txId (Committed TxValid ())
    -- CurrentSlotReq -> CurrentSlotResp Slot
    -- CurrentTimeReq -> CurrentTimeResp POSIXTime
    -- ExposeEndpointReq ActiveEndpoint -> ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    -- PosixTimeRangeToContainedSlotRangeReq POSIXTimeRange -> PosixTimeRangeToContainedSlotRangeResp (Either SlotConversionError SlotRange)
    _ -> pure $ OwnContractInstanceIdResp contractEnv.ceContractInstanceId

  printLog Debug $ show resp
  pure resp

-- | This is not identical to the real balancing, we only do a pre-balance at this stage
balanceTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  ContractEnvironment ->
  UnbalancedTx ->
  Eff effs BalanceTxResponse
balanceTx contractEnv unbalancedTx = do
  -- TODO: Handle paging
  -- (_, Page {pageItems}) <-
  --   chainIndexQueryMany $
  --     ChainIndexClient.getUtxoAtAddress $
  --       addressCredential ownAddress
  -- chainIndexTxOuts <- traverse (chainIndexQueryOne . ChainIndexClient.getTxOut) pageItems
  -- let utxos =
  --       Map.fromList $
  --         catMaybes $ zipWith (\oref txout -> (,) <$> Just oref <*> txout) pageItems chainIndexTxOuts

  eitherPreBalancedTx <-
    PreBalance.preBalanceTxIO
      contractEnv.cePABConfig
      (Ledger.pubKeyHash contractEnv.ceOwnPubKey)
      unbalancedTx

  case eitherPreBalancedTx of
    Left err -> pure $ BalanceTxFailed (InsufficientFunds err)
    Right tx -> pure $ BalanceTxSuccess $ Right tx

-- | This step would build tx files, write them to disk and submit them to the chain
writeBalancedTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  ContractEnvironment ->
  CardanoTx ->
  Eff effs WriteBalancedTxResponse
writeBalancedTx _ (Left _) = error "Cannot handle cardano api tx"
writeBalancedTx contractEnv (Right tx) = do
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
      let ownPkh = Ledger.pubKeyHash contractEnv.ceOwnPubKey
      let requiredSigners = Map.keys $ tx ^. Tx.signatures

      CardanoCLI.uploadFiles contractEnv.cePABConfig

      CardanoCLI.buildTx contractEnv.cePABConfig ownPkh CardanoCLI.BuildAuto tx
      CardanoCLI.signTx contractEnv.cePABConfig tx requiredSigners

      result <-
        if contractEnv.cePABConfig.pcDryRun
          then pure Nothing
          else CardanoCLI.submitTx contractEnv.cePABConfig tx

      case result of
        Just err -> pure $ WriteBalancedTxFailed $ OtherError err
        Nothing -> pure $ WriteBalancedTxSuccess $ Right tx
