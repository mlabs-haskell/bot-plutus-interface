{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BotPlutusInterface.Contract (runContract, handleContract) where

import BotPlutusInterface.Balance qualified as Balance
import BotPlutusInterface.BodyBuilder qualified as BodyBuilder
import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.CardanoNode.Effects (NodeQuery (UtxosAt))
import BotPlutusInterface.Collateral qualified as Collateral
import BotPlutusInterface.Effects (
  PABEffect,
  ShellArgs (ShellArgs, cmdArgs, cmdName, cmdOutParser),
  callCommand,
  createDirectoryIfMissing,
  getInMemCollateral,
  handleContractLog,
  handlePABEffect,
  logToContract,
  minUtxo,
  posixTimeRangeToContainedSlotRange,
  posixTimeToSlot,
  posixTimeToSlotLength,
  printBpiLog,
  queryChainIndex,
  queryNode,
  readFileTextEnvelope,
  saveBudget,
  setInMemCollateral,
  slotToPOSIXTime,
  threadDelay,
  uploadDir,
 )
import BotPlutusInterface.Files (DummyPrivKey (FromSKey, FromVKey))
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (
  CollateralUtxo (CollateralUtxo),
  ContractEnvironment (ContractEnvironment, ceContractInstanceId, cePABConfig),
  LogLevel (Debug, Notice, Warn),
  LogType (CollateralLog, PABLog),
  PABConfig (
    pcCollectStats,
    pcDryRun,
    pcNetwork,
    pcOwnPubKeyHash,
    pcOwnStakePubKeyHash,
    pcProtocolParams,
    pcScriptFileDir,
    pcSigningKeyFileDir,
    pcTipPollingInterval,
    pcTxStatusPolling
  ),
  Tip (block, slot),
  TxStatusPolling (spBlocksTimeOut, spInterval),
  collateralValue,
  ownAddress,
  pcCollateralSize,
  pcOwnPubKeyHash,
 )
import Cardano.Api (
  AsType (AsBabbageEra, AsTxBody),
  EraInMode (BabbageEraInCardanoMode),
  Tx (Tx),
  TxOut (TxOut),
  txOutValueToValue,
 )
import Cardano.Api qualified as CApi
import Cardano.Prelude (fromMaybe, liftA2)
import Control.Lens (preview, (.~), (^.))
import Control.Monad (join, unless, void, when)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, runM, subsume, type (~>))
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Writer (Writer (Tell))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, eitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (ToJSON, Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.Extras (encodeByteString)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.Either.Combinators (maybeToLeft, swapEither)
import Data.Function (fix, (&))
import Data.Kind (Type)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Vector qualified as V
import Ledger (POSIXTime, Params (Params), getCardanoTxId)
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain (UnbalancedTx (UnbalancedCardanoTx, UnbalancedEmulatorTx), tx)
import Ledger.Slot (Slot (Slot))
import Ledger.TimeSlot (nominalDiffTimeToPOSIXTime)
import Ledger.Tx (CardanoTx (CardanoApiTx, EmulatorTx), outputs)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI.Internal (fromCardanoValue)
import Plutus.ChainIndex.TxIdState (fromTx, transactionStatus)
import Plutus.ChainIndex.Types (RollbackState (Unknown), TxIdState, TxStatus)
import Plutus.Contract.Checkpoint (Checkpoint (AllocateKey, DoCheckpoint, Retrieve, Store))
import Plutus.Contract.Effects (
  BalanceTxResponse (BalanceTxFailed, BalanceTxSuccess),
  ChainIndexQuery (TxFromTxId),
  PABReq (
    AdjustUnbalancedTxReq,
    AwaitSlotReq,
    AwaitTimeReq,
    AwaitTxOutStatusChangeReq,
    AwaitTxStatusChangeReq,
    AwaitUtxoProducedReq,
    AwaitUtxoSpentReq,
    BalanceTxReq,
    ChainIndexQueryReq,
    CurrentChainIndexSlotReq,
    CurrentNodeClientSlotReq,
    CurrentNodeClientTimeRangeReq,
    CurrentTimeReq,
    ExposeEndpointReq,
    GetParamsReq,
    OwnAddressesReq,
    OwnContractInstanceIdReq,
    PosixTimeRangeToContainedSlotRangeReq,
    WriteBalancedTxReq,
    YieldUnbalancedTxReq
  ),
  PABResp (
    AdjustUnbalancedTxResp,
    AwaitSlotResp,
    AwaitTimeResp,
    AwaitTxStatusChangeResp,
    BalanceTxResp,
    ChainIndexQueryResp,
    CurrentNodeClientSlotResp,
    CurrentNodeClientTimeRangeResp,
    CurrentTimeResp,
    GetParamsResp,
    OwnAddressesResp,
    OwnContractInstanceIdResp,
    PosixTimeRangeToContainedSlotRangeResp,
    WriteBalancedTxResp
  ),
  WriteBalancedTxResponse (WriteBalancedTxFailed, WriteBalancedTxSuccess),
  _TxIdResponse,
 )
import Plutus.Contract.Resumable (Resumable (RRequest, RSelect, RZero))
import Plutus.Contract.Types (Contract (Contract), ContractEffs)
import PlutusTx.Builtins (fromBuiltin)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))
import Prettyprinter qualified as PP
import Wallet.API qualified as WAPI
import Prelude

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (ToJSON w, Monoid w) =>
  ContractEnvironment w ->
  Contract w s e a ->
  IO (Either e a)
runContract contractEnv (Contract effs) = do
  -- try to create collateral before any contract is executed
  res <- crateCollateralUtxo
  case res of
    Left e -> error $ mkError e
    Right () -> runUserContract
  where
    crateCollateralUtxo =
      runM $ handlePABEffect @w contractEnv (handleCollateral contractEnv)

    runUserContract =
      runM $
        handlePABEffect @w contractEnv $
          raiseEnd $
            handleContract contractEnv effs

    mkError e =
      let collateralAmt = pcCollateralSize $ cePABConfig contractEnv
       in "Tried to create collateral UTxO with " <> show collateralAmt
            <> " lovealces, but failed:\n"
            <> show e
            <> "\nContract execution aborted."

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
                [ pretty (show k) <+> ": "
                , pretty v
                ]
        )
      $ KeyMap.toList obj
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
handlePABReq contractEnv@ContractEnvironment {cePABConfig} req = do
  printBpiLog @w (Debug [PABLog]) $ pretty req
  resp <- case req of
    ----------------------
    -- Handled requests --
    ----------------------
    GetParamsReq ->
      let pparams = fromMaybe (error "GetParamsReq: Must have ProtocolParamaneters in PABConfig") $ pcProtocolParams cePABConfig
          netId = pcNetwork cePABConfig
          -- FIXME: Compute SlotConfig properly
          -- Ideally plutus-apps drops slotConfig from the env, as it shouldn't exist
          slotConfig = def
          pparamsInEra = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams
       in return $ GetParamsResp $ Params slotConfig pparamsInEra netId
    OwnAddressesReq ->
      pure
        . OwnAddressesResp
        . nonEmptySingleton
        $ Ledger.pubKeyHashAddress
          (PaymentPubKeyHash (pcOwnPubKeyHash cePABConfig))
          (Ledger.stakePubKeyHashCredential <$> pcOwnStakePubKeyHash cePABConfig)
    OwnContractInstanceIdReq ->
      pure $ OwnContractInstanceIdResp (ceContractInstanceId contractEnv)
    ChainIndexQueryReq query ->
      ChainIndexQueryResp <$> queryChainIndex @w query
    BalanceTxReq unbalancedTx ->
      BalanceTxResp <$> balanceTx @w contractEnv unbalancedTx
    WriteBalancedTxReq tx' ->
      WriteBalancedTxResp <$> writeBalancedTx @w contractEnv tx'
    AwaitSlotReq s -> AwaitSlotResp <$> awaitSlot @w contractEnv s
    AwaitTimeReq t -> AwaitTimeResp <$> awaitTime @w contractEnv t
    CurrentNodeClientSlotReq -> CurrentNodeClientSlotResp <$> currentSlot @w contractEnv
    CurrentTimeReq -> CurrentTimeResp <$> currentTime @w contractEnv
    PosixTimeRangeToContainedSlotRangeReq posixTimeRange ->
      either (error . show) (PosixTimeRangeToContainedSlotRangeResp . Right)
        <$> posixTimeRangeToContainedSlotRange @w posixTimeRange
    AwaitTxStatusChangeReq txId -> AwaitTxStatusChangeResp txId <$> awaitTxStatusChange @w contractEnv txId
    AdjustUnbalancedTxReq unbalancedTx -> AdjustUnbalancedTxResp <$> adjustUnbalancedTx' @w @effs unbalancedTx
    CurrentNodeClientTimeRangeReq -> do
      t <- currentTime @w contractEnv
      slotLength <- fromRight (error "Failed to query slot length") <$> posixTimeToSlotLength @w t
      return $ CurrentNodeClientTimeRangeResp (t, t + nominalDiffTimeToPOSIXTime slotLength)
    ------------------------
    -- Unhandled requests --
    ------------------------
    AwaitUtxoSpentReq _ -> error ("Unsupported PAB effect: " ++ show req)
    AwaitUtxoProducedReq _ -> error ("Unsupported PAB effect: " ++ show req)
    AwaitTxOutStatusChangeReq _ -> error ("Unsupported PAB effect: " ++ show req)
    ExposeEndpointReq _ -> error ("Unsupported PAB effect: " ++ show req)
    YieldUnbalancedTxReq _ -> error ("Unsupported PAB effect: " ++ show req)
    CurrentChainIndexSlotReq -> error ("Unsupported PAB effect: " ++ show req)

  printBpiLog @w (Debug [PABLog]) $ pretty resp
  pure resp

adjustUnbalancedTx' ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  UnbalancedTx ->
  Eff effs (Either Tx.ToCardanoError UnbalancedTx)
adjustUnbalancedTx' unbalancedTx = runEitherT $ do
  updatedOuts <-
    firstEitherT (Tx.TxBodyError . show) $
      newEitherT $
        sequence <$> traverse (minUtxo @w) (unbalancedTx ^. tx . outputs)

  return $ unbalancedTx & (tx . outputs .~ updatedOuts)

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
  printBpiLog @w (Debug [PABLog]) $ pretty $ "Awaiting status change for " ++ show txId

  let txStatusPolling = pcTxStatusPolling (cePABConfig contractEnv)
      pollInterval = fromIntegral $ spInterval txStatusPolling
      pollTimeout = spBlocksTimeOut txStatusPolling
      cutOffBlock = checkStartedBlock + fromIntegral pollTimeout

  fix $ \loop -> do
    (currBlock, currSlot) <- currentTip contractEnv

    helperLog $
      "Current block: " ++ show currBlock
        ++ ", current slot: "
        ++ show currSlot

    txStatus <- getStatus
    case (txStatus, currBlock > cutOffBlock) of
      (status, True) -> do
        helperLog . mconcat . fmap mconcat $
          [ ["Timeout for waiting `TxId ", show txId, "` status change reached"]
          , [" - waited ", show pollTimeout, " blocks."]
          , [" Current status: ", show status]
          ]
        pure status
      (Unknown, _) -> do
        threadDelay @w pollInterval
        loop
      (status, _) -> pure status
  where
    getStatus = do
      mTx <- queryChainIndexForTxState
      case mTx of
        Nothing -> do
          helperLog $ "TxId " ++ show txId ++ " not found in index"
          pure Unknown
        Just txState -> do
          helperLog $ "TxId " ++ show txId ++ " found in index, checking status"
          blk <- fromInteger <$> currentBlock contractEnv
          case transactionStatus blk txState txId of
            Left e -> do
              helperLog $ "Status check for TxId " ++ show txId ++ " failed with " ++ show e
              pure Unknown
            Right st -> do
              helperLog $ "Status for TxId " ++ show txId ++ " is " ++ show st
              pure st

    queryChainIndexForTxState :: Eff effs (Maybe TxIdState)
    queryChainIndexForTxState = do
      mTx <- join . preview _TxIdResponse <$> (queryChainIndex @w $ TxFromTxId txId)
      case mTx of
        Just tx' -> do
          blk <- fromInteger <$> currentBlock contractEnv
          pure . Just $ fromTx blk tx'
        Nothing -> pure Nothing

    helperLog = printBpiLog @w (Debug [PABLog]) . pretty

-- | This will FULLY balance a transaction
balanceTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  UnbalancedTx ->
  Eff effs BalanceTxResponse
balanceTx _ UnbalancedCardanoTx {} = pure $ BalanceTxFailed $ WAPI.OtherError "CardanoBuildTx is not supported"
balanceTx contractEnv unbalancedTx@UnbalancedEmulatorTx {} = do
  let pabConf = cePABConfig contractEnv

  result <- handleCollateral @w contractEnv

  case result of
    Left e -> pure $ BalanceTxFailed e
    _ -> do
      uploadDir @w (pcSigningKeyFileDir pabConf)
      eitherT (pure . BalanceTxFailed) (pure . BalanceTxSuccess . EmulatorTx) $
        Balance.balanceTxIO @w
          pabConf
          (pcOwnPubKeyHash pabConf)
          unbalancedTx

fromCardanoTx :: CardanoTx -> Tx.Tx
fromCardanoTx (CardanoApiTx _) = error "Cannot handle cardano api tx"
fromCardanoTx (EmulatorTx tx') = tx'

-- | This step would build tx files, write them to disk and submit them to the chain
writeBalancedTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  CardanoTx ->
  Eff effs WriteBalancedTxResponse
writeBalancedTx contractEnv cardanoTx = do
  let pabConf = cePABConfig contractEnv
      tx' = fromCardanoTx cardanoTx
  uploadDir @w (pcSigningKeyFileDir pabConf)
  createDirectoryIfMissing @w False (Text.unpack (pcScriptFileDir pabConf))

  eitherT (pure . WriteBalancedTxFailed . WAPI.OtherError) (pure . WriteBalancedTxSuccess . CardanoApiTx) $ do
    void $ firstEitherT (Text.pack . show) $ newEitherT $ Files.writeAll @w pabConf tx'
    lift $ uploadDir @w (pcScriptFileDir pabConf)

    privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf

    let requiredSigners = Map.keys $ tx' ^. Tx.signatures
        skeys = Map.filter (\case FromSKey _ -> True; FromVKey _ -> False) privKeys
        (presentPubKeys, missingPubKeys) = partition ((`Map.member` skeys) . Ledger.pubKeyHash) requiredSigners

    txBudget <- BodyBuilder.runInEstimationEffect @w tx' id $ BodyBuilder.buildAndEstimateBudget @w pabConf tx'

    -- TODO: This whole part is hacky and we should remove it.
    let path = Text.unpack $ Files.txFilePath pabConf "raw" (Tx.txId tx')
    -- We read back the tx from file as tx currently has the wrong id (but the one we create with cardano-cli is correct)
    babbageBody <- firstEitherT (Text.pack . show) $ newEitherT $ readFileTextEnvelope @w (AsTxBody AsBabbageEra) path
    let cardanoApiTx = Tx.SomeTx (Tx babbageBody []) BabbageEraInCardanoMode

    lift . printBpiLog @w (Debug [PABLog]) $ viaShow presentPubKeys
    lift . printBpiLog @w (Debug [PABLog]) $ viaShow missingPubKeys
    lift . printBpiLog @w (Debug [PABLog]) $ viaShow requiredSigners

    let signingHappened = not $ null presentPubKeys
    when signingHappened $
      newEitherT $ CardanoCLI.signTx @w pabConf tx' presentPubKeys

    let fullySignable = null missingPubKeys
        cardanoTxId = Ledger.getCardanoTxId $ Tx.CardanoApiTx cardanoApiTx

    unless fullySignable $
      lift . printBpiLog @w (Warn [PABLog]) . PP.vsep $
        [ "Not all required signatures have signing key files. Please sign and submit the tx manually:"
        , "Unsigned tx file:" <+> pretty (Files.txFilePath pabConf "raw" (Tx.txId tx'))
        ]
          ++ if not $ null presentPubKeys
            then
              [ "Some signatures were able to sign, partially signed tx available here:"
              , "Partially signed tx file:" <+> pretty (Files.txFilePath pabConf "signed" cardanoTxId)
              ]
            else ["Missing Signatories (pkh):" <+> pretty (Text.unwords (map pkhToText missingPubKeys))]

    when (pcCollectStats pabConf && fullySignable) $
      lift $ saveBudget @w (Tx.txId tx') txBudget

    when (not (pcDryRun pabConf) && fullySignable) $ do
      newEitherT $ CardanoCLI.submitTx @w pabConf tx'

    -- We need to replace the outfile we created at the previous step, as it currently still has the old (incorrect) id
    mvFiles (Files.txFilePath pabConf "raw" (Tx.txId tx')) (Files.txFilePath pabConf "raw" cardanoTxId)
    when signingHappened $
      cpFiles (Files.txFilePath pabConf "signed" (Tx.txId tx')) (Files.txFilePath pabConf "signed" cardanoTxId)

    pure cardanoApiTx
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
    cpFiles :: Text -> Text -> EitherT Text (Eff effs) ()
    cpFiles src dst =
      newEitherT $
        callCommand @w
          ShellArgs
            { cmdName = "cp"
            , cmdArgs = [src, dst]
            , cmdOutParser = const ()
            }

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
  threadDelay @w (fromIntegral (pcTipPollingInterval (cePABConfig contractEnv)))
  tip <- CardanoCLI.queryTip @w $ cePABConfig contractEnv
  case tip of
    Right tip'
      | n < slot tip' -> pure $ Slot (slot tip')
    _ -> awaitSlot contractEnv s

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

type Block = Integer

currentTip ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs (Block, Slot)
currentTip ContractEnvironment {cePABConfig} = do
  tip <-
    either (error . Text.unpack) id
      <$> CardanoCLI.queryTip @w cePABConfig
  pure $ liftA2 (,) block (Slot . slot) tip

currentSlot ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs Slot
currentSlot = fmap snd . currentTip

currentBlock ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs Block
currentBlock = fmap fst . currentTip

currentTime ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs POSIXTime
currentTime contractEnv =
  currentSlot @w contractEnv
    >>= slotToPOSIXTime @w
    >>= either (error . show) pure

-- | Check if collateral in contract environment, if not - create and set to environment
handleCollateral ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs (Either WAPI.WalletAPIError ())
handleCollateral cEnv = do
  let ownPkh = pcOwnPubKeyHash $ cePABConfig cEnv
  result <- (fmap swapEither . runEitherT) $
    do
      let helperLog :: PP.Doc () -> ExceptT CollateralUtxo (Eff effs) ()
          helperLog msg = newEitherT $ Right <$> printBpiLog @w (Debug [CollateralLog]) msg

      collateralNotInMem <-
        newEitherT $
          maybeToLeft ("PKH: " <> pretty ownPkh <> ". Collateral UTxO not found in contract env.")
            <$> getInMemCollateral @w

      helperLog collateralNotInMem

      collateralNotInWallet <- newEitherT $ swapEither <$> findCollateralAtOwnPKH cEnv

      helperLog
        ( "PKH: " <> pretty ownPkh <> ". Collateral UTxO not found or failed to be found in wallet: "
            <> pretty collateralNotInWallet
        )

      helperLog ("PKH: " <> pretty ownPkh <> ". Creating collateral UTxO.")

      notCreatedCollateral <- newEitherT $ swapEither <$> makeCollateral @w cEnv

      helperLog
        ( "PKH: " <> pretty ownPkh <> ". Failed to create collateral UTxO: "
            <> pretty notCreatedCollateral
        )

      pure
        ( "PKH: " <> show ownPkh <> ". Failed to create collateral UTxO: "
            <> show notCreatedCollateral
        )

  case result of
    Right collteralUtxo ->
      setInMemCollateral @w collteralUtxo
        >> Right
          <$> printBpiLog @w
            (Debug [CollateralLog])
            ("PKH: " <> pretty ownPkh <> ". Successfully set the collateral utxo in env.")
    Left err ->
      pure $
        Left $
          WAPI.OtherError $
            T.pack $
              "PKH: " <> show ownPkh <> ". Failed to make collateral: " <> show err

{- | Create collateral UTxO by submitting Tx.
  Then try to find created UTxO at own PKH address.
-}
makeCollateral ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs (Either WAPI.WalletAPIError CollateralUtxo)
makeCollateral cEnv = runEitherT $ do
  lift $ printBpiLog @w (Notice [CollateralLog]) "Making collateral"

  let pabConf = cePABConfig cEnv

  -- TODO: Enforce existence of pparams at the beginning
  pparams <- maybe (error "Must have ProtocolParameters in PABConfig") return (pcProtocolParams pabConf)
  let pparamsInEra = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams

  unbalancedTx <-
    firstEitherT (WAPI.OtherError . Text.pack . show) $
      hoistEither $ Collateral.mkCollateralTx pabConf (error "We should not use SlotConfig anywhere") pparamsInEra -- FIXME: SlotConfig shennanigans
  balancedTx <-
    Balance.balanceTxIO' @w
      Balance.defaultBalanceConfig {Balance.bcSeparateChange = True}
      pabConf
      (pcOwnPubKeyHash pabConf)
      unbalancedTx

  wbr <- lift $ writeBalancedTx cEnv (EmulatorTx balancedTx)
  case wbr of
    WriteBalancedTxFailed e -> throwE . WAPI.OtherError . Text.pack $ "Failed to create collateral output: " <> show e
    WriteBalancedTxSuccess cTx -> do
      status <- lift $ awaitTxStatusChange cEnv (getCardanoTxId cTx)
      lift $ printBpiLog @w (Notice [CollateralLog]) $ "Collateral Tx Status: " <> pretty status
      newEitherT $ findCollateralAtOwnPKH cEnv

-- | Finds a collateral present at user's address
findCollateralAtOwnPKH ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  Eff effs (Either WAPI.WalletAPIError CollateralUtxo)
findCollateralAtOwnPKH cEnv =
  runEitherT $
    CollateralUtxo <$> do
      let pabConf = cePABConfig cEnv

      changeAddr <- hoistEither $ first WAPI.ToCardanoError $ ownAddress pabConf

      r <-
        firstEitherT (WAPI.OtherError . Text.pack . show) $
          newEitherT $ queryNode @w (UtxosAt changeAddr)
      let refsAndValues = Map.toList $ txOutvalue <$> r
      hoistEither $ case filter isAcceptableCollateral refsAndValues of
        [] -> Left $ WAPI.OtherError "Couldn't find collateral UTxO"
        ((oref, _) : _) -> Right oref
  where
    isAcceptableCollateral (_, v) = fromCardanoValue v == collateralValue (cePABConfig cEnv)
    txOutvalue (TxOut _ v _ _) = txOutValueToValue v

{- | Construct a 'NonEmpty' list from a single element.
 Should be replaced by NonEmpty.singleton after updating to base 4.15
-}
nonEmptySingleton :: a -> NonEmpty a
nonEmptySingleton = (:| [])
