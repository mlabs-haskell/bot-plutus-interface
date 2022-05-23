{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.Contract (runContract, handleContract) where

import BotPlutusInterface.Balance qualified as PreBalance
import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (
  PABEffect,
  ShellArgs (..),
  callCommand,
  convertTimeRangeToSlotRange,
  createDirectoryIfMissing,
  estimateBudget,
  handlePABEffect,
  logToContract,
  posixTimeToSlot,
  printLog,
  queryChainIndex,
  readFileTextEnvelope,
  saveBudget,
  slotToPOSIXTime,
  threadDelay,
  uploadDir,
 )
import BotPlutusInterface.Files (DummyPrivKey (FromSKey, FromVKey))
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.TimeSlot (ToWhichSlotTime (ToEndTime), stackExchangeConvert)
import BotPlutusInterface.Types (
  ContractEnvironment (..),
  LogLevel (Debug, Warn),
  Tip (block, slot),
  TxFile (Signed),
 )
import Cardano.Api (AsType (..), EraInMode (..), Tx (Tx))
import Control.Lens (preview, (^.))
import Control.Monad (join, void, when)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, runM, subsume, type (~>))
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Extras.Log (handleLogIgnore)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Writer (Writer (Tell))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, eitherT, firstEitherT, newEitherT)
import Data.Aeson (ToJSON, Value)
import Data.Aeson.Extras (encodeByteString)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceM)
import Ledger (POSIXTime)
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Slot (Slot (Slot))
import Ledger.TimeSlot (SlotConversionError, posixTimeToEnclosingSlot, slotToEndPOSIXTime)
import Ledger.Tx (CardanoTx)
import Ledger.Tx qualified as Tx
import Plutus.ChainIndex.TxIdState (fromTx, transactionStatus)
import Plutus.ChainIndex.Types (RollbackState (..), TxIdState, TxStatus)
import Plutus.Contract.Checkpoint (Checkpoint (..))
import Plutus.Contract.Effects (
  BalanceTxResponse (..),
  ChainIndexQuery (..),
  PABReq (..),
  PABResp (..),
  WriteBalancedTxResponse (..),
  _TxIdResponse,
 )
import Plutus.Contract.Resumable (Resumable (..))
import Plutus.Contract.Types (Contract (..), ContractEffs)
import PlutusTx.Builtins (fromBuiltin)
import Wallet.Emulator.Error (WalletAPIError (..))
import Prelude

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (ToJSON w, Monoid w) =>
  ContractEnvironment w ->
  Contract w s e a ->
  IO (Either e a)
runContract contractEnv (Contract effs) = do
  runM $ handlePABEffect @w contractEnv $ raiseEnd $ handleContract contractEnv effs

handleContract ::
  forall (w :: Type) (e :: Type) (a :: Type).
  (ToJSON w, Monoid w) =>
  ContractEnvironment w ->
  Eff (ContractEffs w e) a ->
  Eff '[PABEffect w] (Either e a)
handleContract contractEnv =
  subsume @(PABEffect w)
    . handleResumable contractEnv
    . handleCheckpointIgnore
    . handleWriter
    . handleLogIgnore @Value
    . runError
    . raiseEnd

handleWriter ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (ToJSON w, Monoid w) =>
  (Member (PABEffect w) effs) =>
  Eff (Writer w ': effs)
    ~> Eff effs
handleWriter =
  interpret
    (\case Tell msg -> logToContract msg)

handleResumable ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ContractEnvironment w ->
  Eff (Resumable PABResp PABReq ': effs) ~> Eff (PABEffect w ': effs)
handleResumable contractEnv =
  reinterpret
    ( \case
        RRequest o -> handlePABReq @w contractEnv o
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
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  PABReq ->
  Eff effs PABResp
handlePABReq contractEnv req = do
  printLog @w Debug $ show req
  resp <- case req of
    ----------------------
    -- Handled requests --
    ----------------------
    OwnPaymentPublicKeyHashReq ->
      pure $ OwnPaymentPublicKeyHashResp $ PaymentPubKeyHash contractEnv.cePABConfig.pcOwnPubKeyHash
    OwnContractInstanceIdReq ->
      pure $ OwnContractInstanceIdResp (ceContractInstanceId contractEnv)
    ChainIndexQueryReq query ->
      ChainIndexQueryResp <$> queryChainIndex @w query
    BalanceTxReq unbalancedTx ->
      BalanceTxResp <$> balanceTx @w contractEnv unbalancedTx
    WriteBalancedTxReq tx ->
      WriteBalancedTxResp <$> writeBalancedTx @w contractEnv tx
    AwaitSlotReq s -> AwaitSlotResp <$> awaitSlot @w contractEnv s
    AwaitTimeReq t -> AwaitTimeResp <$> awaitTime @w contractEnv t
    CurrentSlotReq -> CurrentSlotResp <$> currentSlot @w contractEnv
    CurrentTimeReq -> CurrentTimeResp <$> currentTime @w contractEnv
    PosixTimeRangeToContainedSlotRangeReq posixTimeRange ->
      PosixTimeRangeToContainedSlotRangeResp
        <$> posixTimeRangeToContainedSlotRange_ @w posixTimeRange
    -- PosixTimeRangeToContainedSlotRangeReq posixTimeRange ->
    --   pure $
    --     PosixTimeRangeToContainedSlotRangeResp $
    --       Right $
    --         posixTimeRangeToContainedSlotRange contractEnv.cePABConfig.pcSlotConfig posixTimeRange
    AwaitTxStatusChangeReq txId -> AwaitTxStatusChangeResp txId <$> awaitTxStatusChange @w contractEnv txId
    ------------------------
    -- Unhandled requests --
    ------------------------
    -- AwaitTimeReq t -> pure $ AwaitTimeResp t
    -- AwaitUtxoSpentReq txOutRef -> pure $ AwaitUtxoSpentResp ChainIndexTx
    -- AwaitUtxoProducedReq Address -> pure $ AwaitUtxoProducedResp (NonEmpty ChainIndexTx)
    -- AwaitTxOutStatusChangeReq TxOutRef
    -- ExposeEndpointReq ActiveEndpoint -> ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    -- YieldUnbalancedTxReq UnbalancedTx
    unsupported -> error ("Unsupported PAB effect: " ++ show unsupported)

  printLog @w Debug $ show resp
  pure resp

awaitTxStatusChange ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Ledger.TxId ->
  Eff effs TxStatus
awaitTxStatusChange contractEnv txId = do
  -- The depth (in blocks) after which a transaction cannot be rolled back anymore (from Plutus.ChainIndex.TxIdState)
  let chainConstant = 8

  mTx <- queryChainIndexForTxState
  case mTx of
    Nothing -> pure Unknown
    Just txState -> do
      printLog @w Debug $ "Found transaction in node, waiting " ++ show chainConstant ++ " blocks for it to settle."
      awaitNBlocks @w contractEnv (chainConstant + 1)
      -- Check if the tx is still present in chain-index, in case of a rollback
      -- we might not find it anymore.
      ciTxState' <- queryChainIndexForTxState
      case ciTxState' of
        Nothing -> pure Unknown
        Just _ -> do
          blk <- fromInteger <$> currentBlock contractEnv
          -- This will set the validity correctly based on the txState.
          -- The tx will always be committed, as we wait for chainConstant + 1 blocks
          let status = transactionStatus blk txState txId
          pure $ fromRight Unknown status
  where
    queryChainIndexForTxState :: Eff effs (Maybe TxIdState)
    queryChainIndexForTxState = do
      mTx <- join . preview _TxIdResponse <$> (queryChainIndex @w $ TxFromTxId txId)
      case mTx of
        Just tx -> do
          blk <- fromInteger <$> currentBlock contractEnv
          pure . Just $ fromTx blk tx
        Nothing -> pure Nothing

-- | This will FULLY balance a transaction
balanceTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  UnbalancedTx ->
  Eff effs BalanceTxResponse
balanceTx contractEnv unbalancedTx = do
  let pabConf = contractEnv.cePABConfig
  uploadDir @w pabConf.pcSigningKeyFileDir
  eitherPreBalancedTx <-
    PreBalance.balanceTxIO @w
      pabConf
      pabConf.pcOwnPubKeyHash
      unbalancedTx

  pure $ either (BalanceTxFailed . InsufficientFunds) (BalanceTxSuccess . Right) eitherPreBalancedTx

-- | This step would build tx files, write them to disk and submit them to the chain
writeBalancedTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  CardanoTx ->
  Eff effs WriteBalancedTxResponse
writeBalancedTx _ (Left _) = error "Cannot handle cardano api tx"
writeBalancedTx contractEnv (Right tx) = do
  let pabConf = contractEnv.cePABConfig
  uploadDir @w pabConf.pcSigningKeyFileDir
  createDirectoryIfMissing @w False (Text.unpack pabConf.pcScriptFileDir)

  eitherT (pure . WriteBalancedTxFailed . OtherError) (pure . WriteBalancedTxSuccess . Left) $ do
    void $ firstEitherT (Text.pack . show) $ newEitherT $ Files.writeAll @w pabConf tx
    lift $ uploadDir @w pabConf.pcScriptFileDir

    privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf

    let requiredSigners = Map.keys $ tx ^. Tx.signatures
        skeys = Map.filter (\case FromSKey _ -> True; FromVKey _ -> False) privKeys
        signable = all ((`Map.member` skeys) . Ledger.pubKeyHash) requiredSigners

    void $ newEitherT $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys tx

    -- TODO: This whole part is hacky and we should remove it.
    let path = Text.unpack $ Files.txFilePath pabConf "raw" (Tx.txId tx)
    -- We read back the tx from file as tx currently has the wrong id (but the one we create with cardano-cli is correct)
    alonzoBody <- firstEitherT (Text.pack . show) $ newEitherT $ readFileTextEnvelope @w (AsTxBody AsAlonzoEra) path
    let cardanoTx = Tx.SomeTx (Tx alonzoBody []) AlonzoEraInCardanoMode

    if signable
      then newEitherT $ CardanoCLI.signTx @w pabConf tx requiredSigners
      else
        lift . printLog @w Warn . Text.unpack . Text.unlines $
          [ "Not all required signatures have signing key files. Please sign and submit the tx manually:"
          , "Tx file: " <> Files.txFilePath pabConf "raw" (Tx.txId tx)
          , "Signatories (pkh): " <> Text.unwords (map pkhToText requiredSigners)
          ]

    when (not pabConf.pcDryRun && signable) $ do
      newEitherT $ CardanoCLI.submitTx @w pabConf tx

    -- We need to replace the outfile we created at the previous step, as it currently still has the old (incorrect) id
    let cardanoTxId = Ledger.getCardanoTxId $ Left cardanoTx
        signedSrcPath = Files.txFilePath pabConf "signed" (Tx.txId tx)
        signedDstPath = Files.txFilePath pabConf "signed" cardanoTxId
    mvFiles (Files.txFilePath pabConf "raw" (Tx.txId tx)) (Files.txFilePath pabConf "raw" cardanoTxId)
    when signable $ mvFiles signedSrcPath signedDstPath

    when contractEnv.cePABConfig.pcCollectStats $
      collectBudgetStats cardanoTxId signedDstPath

    pure cardanoTx
  where
    mvFiles :: Text -> Text -> EitherT Text (Eff effs) ()
    mvFiles src dst =
      newEitherT $
        callCommand @w
          ShellArgs
            { cmdName = "mv"
            , cmdArgs = [src, dst]
            , cmdOutParser = const ()
            }

    collectBudgetStats txId txPath = do
      let path = Text.unpack txPath
      b <- firstEitherT (Text.pack . show) $ newEitherT $ estimateBudget @w (Signed path)
      void $ newEitherT (Right <$> saveBudget @w txId b)

pkhToText :: Ledger.PubKey -> Text
pkhToText = encodeByteString . fromBuiltin . Ledger.getPubKeyHash . Ledger.pubKeyHash

{- | Wait at least until the given slot. The slot number only changes when a new block is appended
 to the chain so it waits for at least one block
-}
awaitSlot ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Slot ->
  Eff effs Slot
awaitSlot contractEnv s@(Slot n) = do
  threadDelay @w (fromIntegral contractEnv.cePABConfig.pcTipPollingInterval)
  tip <- CardanoCLI.queryTip @w contractEnv.cePABConfig
  case tip of
    Right tip'
      | n < tip'.slot -> pure $ Slot tip'.slot
    _ -> awaitSlot contractEnv s

-- | Wait for n Blocks.
awaitNBlocks ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Integer ->
  Eff effs ()
awaitNBlocks contractEnv n = do
  current <- currentBlock contractEnv
  go current
  where
    go :: Integer -> Eff effs ()
    go start = do
      threadDelay @w (fromIntegral contractEnv.cePABConfig.pcTipPollingInterval)
      tip <- CardanoCLI.queryTip @w contractEnv.cePABConfig
      case tip of
        Right tip'
          | start + n <= tip'.block -> pure ()
        _ -> go start

{- | Wait at least until the given time. Uses the awaitSlot under the hood, so the same constraints
 are applying here as well.
-}
awaitTime ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  POSIXTime ->
  Eff effs POSIXTime
awaitTime ce pTime = do
  startingTip <- CardanoCLI.queryTip @w ce.cePABConfig
  traceM $ "Starting tip: " ++ show startingTip

  slotFromTime <- posixTimeToSlot @w pTime >>= either (error . show) return
  traceM $ "Converting " ++ show pTime ++ " to slot:"
  traceM $ "- with node queries: " ++ show slotFromTime
  traceM $ "- with stack exchng: " ++ show (stackExchangeConvert pTime)
  traceM $ "- with ledger stuff: " ++ show oldSlotFromTime
  slot' <- awaitSlot ce slotFromTime
  traceM $ "Converting " ++ show slot' ++ " to time:"
  ethTime <- slotToPOSIXTime @w ToEndTime slot'
  time' <- either (error . show) return ethTime
  traceM $ "- with node queries: " ++ show time'
  traceM $ "- with ledger stuff: " ++ show (oldTimeFromSlot slot')

  endTip <- CardanoCLI.queryTip @w ce.cePABConfig
  traceM $ "Current tip: " ++ show endTip
  return time'
  where
    oldSlotFromTime = posixTimeToEnclosingSlot ce.cePABConfig.pcSlotConfig pTime
    oldTimeFromSlot = slotToEndPOSIXTime ce.cePABConfig.pcSlotConfig

currentSlot ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs Slot
currentSlot contractEnv =
  Slot . slot . either (error . Text.unpack) id <$> CardanoCLI.queryTip @w contractEnv.cePABConfig

currentBlock ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs Integer
currentBlock contractEnv =
  block . either (error . Text.unpack) id <$> CardanoCLI.queryTip @w contractEnv.cePABConfig

currentTime ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs POSIXTime
currentTime contractEnv =
  currentSlot @w contractEnv
    >>= slotToPOSIXTime @w ToEndTime
    >>= either (error . show) return

posixTimeRangeToContainedSlotRange_ ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Ledger.POSIXTimeRange ->
  Eff effs (Either SlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRange_ posixTimeRange =
  convertTimeRangeToSlotRange @w posixTimeRange
    >>= either (error . show) (return . Right)
