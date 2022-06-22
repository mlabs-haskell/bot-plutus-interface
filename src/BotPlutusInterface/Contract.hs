{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BotPlutusInterface.Contract (runContract, handleContract) where

import BotPlutusInterface.Balance qualified as PreBalance
import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (
  PABEffect,
  ShellArgs (..),
  callCommand,
  createDirectoryIfMissing,
  estimateBudget,
  handleContractLog,
  handlePABEffect,
  logToContract,
  posixTimeRangeToContainedSlotRange,
  posixTimeToSlot,
  printBpiLog,
  queryChainIndex,
  readFileTextEnvelope,
  saveBudget,
  slotToPOSIXTime,
  threadDelay,
  uploadDir,
 )
import BotPlutusInterface.Files (DummyPrivKey (FromSKey, FromVKey))
import BotPlutusInterface.Files qualified as Files
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
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Writer (Writer (Tell))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, eitherT, firstEitherT, newEitherT)
import Data.Aeson (ToJSON, Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.Extras (encodeByteString)
import Data.Function (fix)
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Ledger (POSIXTime)
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Slot (Slot (Slot))
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
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as PP
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
    . handleContractLog @w
    . runError
    . raiseEnd

instance Pretty Value where
  pretty (String s) = pretty s
  pretty (Number n) = pretty $ show n
  pretty (Bool b) = pretty b
  pretty (Array arr) = PP.list $ pretty <$> V.toList arr
  pretty (Object obj) =
    PP.group
      . PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") ", "
      . map
        ( \(k, v) ->
            PP.hang 2 $
              PP.sep
                [ pretty k <+> ": "
                , pretty v
                ]
        )
      $ HM.toList obj
  pretty Null = "null"

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
  printBpiLog @w Debug $ pretty req
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
      either (error . show) (PosixTimeRangeToContainedSlotRangeResp . Right)
        <$> posixTimeRangeToContainedSlotRange @w posixTimeRange
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

  printBpiLog @w Debug $ pretty resp
  pure resp

{- | Await till transaction status change to something from `Unknown`.
 Uses `chain-index` to query transaction by id.
 Important notes:
 * if transaction is not found in `chain-index` status considered to be `Unknown`
 * if transaction is found but `transactionStatus` failed to make status - status considered to be `Unknown`
 * uses `TxStatusPolling` to set `chain-index` polling interval and number of blocks to wait until timeout,
   if timeout is reached, returns whatever status it was able to get during last check
-}
awaitTxStatusChange ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Ledger.TxId ->
  Eff effs TxStatus
awaitTxStatusChange contractEnv txId = do
  checkStartedBlock <- currentBlock contractEnv
  printBpiLog @w Debug $ pretty $ "Awaiting status change for " ++ show txId

  let txStatusPolling = contractEnv.cePABConfig.pcTxStatusPolling
      pollInterval = fromIntegral $ txStatusPolling.spInterval
      pollTimeout = txStatusPolling.spBlocksTimeOut
      cutOffBlock = checkStartedBlock + fromIntegral pollTimeout

  fix $ \loop -> do
    currBlock <- currentBlock contractEnv
    txStatus <- getStatus
    case (txStatus, currBlock > cutOffBlock) of
      (status, True) -> do
        logDebug . mconcat . fmap mconcat $
          [ ["Timeout for waiting `TxId ", show txId, "` status change reached"]
          , [" - waited ", show pollTimeout, " blocks."]
          , [" Current status: ", show status]
          ]
        return status
      (Unknown, _) -> do
        threadDelay @w pollInterval
        loop
      (status, _) -> return status
  where
    getStatus = do
      mTx <- queryChainIndexForTxState
      case mTx of
        Nothing -> do
          logDebug $ "TxId " ++ show txId ++ " not found in index"
          return Unknown
        Just txState -> do
          logDebug $ "TxId " ++ show txId ++ " found in index, checking status"
          blk <- fromInteger <$> currentBlock contractEnv
          case transactionStatus blk txState txId of
            Left e -> do
              logDebug $ "Status check for TxId " ++ show txId ++ " failed with " ++ show e
              return Unknown
            Right st -> do
              logDebug $ "Status for TxId " ++ show txId ++ " is " ++ show st
              return st

    queryChainIndexForTxState :: Eff effs (Maybe TxIdState)
    queryChainIndexForTxState = do
      mTx <- join . preview _TxIdResponse <$> (queryChainIndex @w $ TxFromTxId txId)
      case mTx of
        Just tx -> do
          blk <- fromInteger <$> currentBlock contractEnv
          pure . Just $ fromTx blk tx
        Nothing -> pure Nothing

    logDebug = printBpiLog @w Debug . pretty

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
        lift . printBpiLog @w Warn . PP.vsep $
          [ "Not all required signatures have signing key files. Please sign and submit the tx manually:"
          , "Tx file:" <+> pretty (Files.txFilePath pabConf "raw" (Tx.txId tx))
          , "Signatories (pkh):" <+> pretty (Text.unwords (map pkhToText requiredSigners))
          ]

    when (pabConf.pcCollectStats && signable) $
      collectBudgetStats (Tx.txId tx) pabConf

    when (not pabConf.pcDryRun && signable) $ do
      newEitherT $ CardanoCLI.submitTx @w pabConf tx

    -- We need to replace the outfile we created at the previous step, as it currently still has the old (incorrect) id
    let cardanoTxId = Ledger.getCardanoTxId $ Left cardanoTx
        signedSrcPath = Files.txFilePath pabConf "signed" (Tx.txId tx)
        signedDstPath = Files.txFilePath pabConf "signed" cardanoTxId
    mvFiles (Files.txFilePath pabConf "raw" (Tx.txId tx)) (Files.txFilePath pabConf "raw" cardanoTxId)
    when signable $ mvFiles signedSrcPath signedDstPath

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

    collectBudgetStats txId pabConf = do
      let path = Text.unpack (Files.txFilePath pabConf "signed" (Tx.txId tx))
      txBudget <-
        firstEitherT toBudgetSaveError $
          newEitherT $ estimateBudget @w (Signed path)
      void $ newEitherT (Right <$> saveBudget @w txId txBudget)

    toBudgetSaveError =
      Text.pack
        . ("Failed to save Tx budgets statistics: " ++)
        . show

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

-- -- | Wait for n Blocks.
-- awaitNBlocks ::
--   forall (w :: Type) (effs :: [Type -> Type]).
--   Member (PABEffect w) effs =>
--   ContractEnvironment w ->
--   Integer ->
--   Eff effs ()
-- awaitNBlocks contractEnv n = do
--   current <- currentBlock contractEnv
--   go current
--   where
--     go :: Integer -> Eff effs ()
--     go start = do
--       threadDelay @w (fromIntegral contractEnv.cePABConfig.pcTipPollingInterval)
--       tip <- CardanoCLI.queryTip @w contractEnv.cePABConfig
--       case tip of
--         Right tip'
--           | start + n <= tip'.block -> pure ()
--         _ -> go start

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
  slotFromTime <- rightOrErr <$> posixTimeToSlot @w pTime
  slot' <- awaitSlot ce slotFromTime
  rightOrErr <$> slotToPOSIXTime @w slot'
  where
    rightOrErr = either (error . show) id

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
    >>= slotToPOSIXTime @w
    >>= either (error . show) return
