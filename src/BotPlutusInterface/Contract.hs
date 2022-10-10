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
  ShellArgs (..),
  callCommand,
  createDirectoryIfMissing,
  estimateBudget,
  getInMemCollateral,
  handleContractLog,
  handlePABEffect,
  logToContract,
  minUtxo,
  posixTimeRangeToContainedSlotRange,
  posixTimeToSlot,
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
  ContractEnvironment (..),
  LogLevel (Debug, Notice, Warn),
  LogType (CollateralLog, PABLog),
  Tip (block, slot),
  TxFile (Signed),
  collateralValue,
 )
import Cardano.Api (
  AsType (..),
  EraInMode (..),
  Tx (Tx),
 )
import Cardano.Prelude (liftA2)
import Control.Lens (preview, (.~), (^.))
import Control.Monad (join, void, when)
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
import Data.Either.Combinators (maybeToLeft, swapEither)
import Data.Function (fix, (&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Vector qualified as V
import Ledger (POSIXTime, getCardanoTxId)
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain (UnbalancedTx (..), tx)
import Ledger.Slot (Slot (Slot))
import Ledger.Tx (CardanoTx (CardanoApiTx, EmulatorTx), outputs)
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
import Wallet.API qualified as WAPI
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
handlePABReq contractEnv req = do
  printBpiLog @w (Debug [PABLog]) $ pretty req
  resp <- case req of
    ----------------------
    -- Handled requests --
    ----------------------
    OwnAddressesReq ->
      pure
        . OwnAddressesResp
        . nonEmptySingleton
        $ Ledger.pubKeyHashAddress
          (PaymentPubKeyHash contractEnv.cePABConfig.pcOwnPubKeyHash)
          contractEnv.cePABConfig.pcOwnStakePubKeyHash
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
    CurrentPABSlotReq -> CurrentPABSlotResp <$> currentSlot @w contractEnv
    CurrentTimeReq -> CurrentTimeResp <$> currentTime @w contractEnv
    PosixTimeRangeToContainedSlotRangeReq posixTimeRange ->
      either (error . show) (PosixTimeRangeToContainedSlotRangeResp . Right)
        <$> posixTimeRangeToContainedSlotRange @w posixTimeRange
    AwaitTxStatusChangeReq txId -> AwaitTxStatusChangeResp txId <$> awaitTxStatusChange @w contractEnv txId
    AdjustUnbalancedTxReq unbalancedTx -> AdjustUnbalancedTxResp <$> adjustUnbalancedTx' @w @effs unbalancedTx
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

  let txStatusPolling = contractEnv.cePABConfig.pcTxStatusPolling
      pollInterval = fromIntegral $ txStatusPolling.spInterval
      pollTimeout = txStatusPolling.spBlocksTimeOut
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
balanceTx _ (UnbalancedTx (Left _) _ _ _) = pure $ BalanceTxFailed $ OtherError "CardanoBuildTx is not supported"
balanceTx contractEnv unbalancedTx@(UnbalancedTx (Right tx') _ _ _) = do
  let pabConf = contractEnv.cePABConfig

  result <- handleCollateral @w contractEnv

  case result of
    Left e -> pure $ BalanceTxFailed e
    _ -> do
      uploadDir @w pabConf.pcSigningKeyFileDir
      eitherBalancedTx <-
        Balance.balanceTxIO' @w
          Balance.defaultBalanceConfig
            { Balance.bcHasScripts = Balance.txUsesScripts tx'
            }
          pabConf
          pabConf.pcOwnPubKeyHash
          unbalancedTx

      pure $ either BalanceTxFailed (BalanceTxSuccess . EmulatorTx) eitherBalancedTx

fromCardanoTx :: CardanoTx -> Tx.Tx
fromCardanoTx (CardanoApiTx _) = error "Cannot handle cardano api tx"
fromCardanoTx (EmulatorTx tx') = tx'
fromCardanoTx (Tx.Both tx' _) = tx'

-- | This step would build tx files, write them to disk and submit them to the chain
writeBalancedTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ContractEnvironment w ->
  CardanoTx ->
  Eff effs WriteBalancedTxResponse
writeBalancedTx contractEnv cardanoTx = do
  let pabConf = contractEnv.cePABConfig
      tx' = fromCardanoTx cardanoTx
  uploadDir @w pabConf.pcSigningKeyFileDir
  createDirectoryIfMissing @w False (Text.unpack pabConf.pcScriptFileDir)

  eitherT (pure . WriteBalancedTxFailed . OtherError) (pure . WriteBalancedTxSuccess . CardanoApiTx) $ do
    void $ firstEitherT (Text.pack . show) $ newEitherT $ Files.writeAll @w pabConf tx'
    lift $ uploadDir @w pabConf.pcScriptFileDir

    privKeys <- newEitherT $ Files.readPrivateKeys @w pabConf

    let requiredSigners = Map.keys $ tx' ^. Tx.signatures
        skeys = Map.filter (\case FromSKey _ -> True; FromVKey _ -> False) privKeys
        signable = all ((`Map.member` skeys) . Ledger.pubKeyHash) requiredSigners

    void $ newEitherT $ BodyBuilder.buildAndEstimateBudget @w pabConf privKeys tx'

    -- TODO: This whole part is hacky and we should remove it.
    let path = Text.unpack $ Files.txFilePath pabConf "raw" (Tx.txId tx')
    -- We read back the tx from file as tx currently has the wrong id (but the one we create with cardano-cli is correct)
    babbageBody <- firstEitherT (Text.pack . show) $ newEitherT $ readFileTextEnvelope @w (AsTxBody AsBabbageEra) path
    let cardanoApiTx = Tx.SomeTx (Tx babbageBody []) BabbageEraInCardanoMode

    if signable
      then newEitherT $ CardanoCLI.signTx @w pabConf tx' requiredSigners
      else
        lift . printBpiLog @w (Warn [PABLog]) . PP.vsep $
          [ "Not all required signatures have signing key files. Please sign and submit the tx manually:"
          , "Tx file:" <+> pretty (Files.txFilePath pabConf "raw" (Tx.txId tx'))
          , "Signatories (pkh):" <+> pretty (Text.unwords (map pkhToText requiredSigners))
          ]

    when (pabConf.pcCollectStats && signable) $
      collectBudgetStats (Tx.txId tx') pabConf

    when (not pabConf.pcDryRun && signable) $ do
      newEitherT $ CardanoCLI.submitTx @w pabConf tx'

    -- We need to replace the outfile we created at the previous step, as it currently still has the old (incorrect) id
    let cardanoTxId = Ledger.getCardanoTxId $ Tx.CardanoApiTx cardanoApiTx
        signedSrcPath = Files.txFilePath pabConf "signed" (Tx.txId tx')
        signedDstPath = Files.txFilePath pabConf "signed" cardanoTxId
    mvFiles (Files.txFilePath pabConf "raw" (Tx.txId tx')) (Files.txFilePath pabConf "raw" cardanoTxId)
    when signable $ mvFiles signedSrcPath signedDstPath

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

    collectBudgetStats txId pabConf = do
      let path = Text.unpack (Files.txFilePath pabConf "signed" txId)
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
currentTip contractEnv = do
  tip <-
    either (error . Text.unpack) id
      <$> CardanoCLI.queryTip @w contractEnv.cePABConfig
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
  result <- (fmap swapEither . runEitherT) $
    do
      let helperLog :: PP.Doc () -> ExceptT CollateralUtxo (Eff effs) ()
          helperLog msg = newEitherT $ Right <$> printBpiLog @w (Debug [CollateralLog]) msg

      collateralNotInMem <-
        newEitherT $
          maybeToLeft "Collateral UTxO not found in contract env."
            <$> getInMemCollateral @w

      helperLog collateralNotInMem

      collateralNotInWallet <- newEitherT $ swapEither <$> findCollateralAtOwnPKH cEnv

      helperLog
        ("Collateral UTxO not found or failed to be found in wallet: " <> pretty collateralNotInWallet)

      helperLog "Creating collateral UTxO."

      notCreatedCollateral <- newEitherT $ swapEither <$> makeCollateral @w cEnv

      helperLog
        ("Failed to create collateral UTxO: " <> pretty notCreatedCollateral)

      pure ("Failed to create collateral UTxO: " <> show notCreatedCollateral)

  case result of
    Right collteralUtxo ->
      setInMemCollateral @w collteralUtxo
        >> Right <$> printBpiLog @w (Debug [CollateralLog]) "successfully set the collateral utxo in env."
    Left err -> pure $ Left $ WAPI.OtherError $ T.pack $ "Failed to make collateral: " <> show err

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

  let pabConf = cEnv.cePABConfig
  unbalancedTx <-
    firstEitherT (WAPI.OtherError . Text.pack . show) $
      hoistEither $ Collateral.mkCollateralTx pabConf

  balancedTx <-
    newEitherT $
      Balance.balanceTxIO' @w
        Balance.defaultBalanceConfig {Balance.bcHasScripts = False, Balance.bcSeparateChange = True}
        pabConf
        pabConf.pcOwnPubKeyHash unbalancedTx

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
          changeAddr =
            Ledger.pubKeyHashAddress
              (PaymentPubKeyHash pabConf.pcOwnPubKeyHash)
              pabConf.pcOwnStakePubKeyHash

      r <-
        firstEitherT (WAPI.OtherError . Text.pack . show) $
          newEitherT $ queryNode @w (UtxosAt changeAddr)
      let refsAndOuts = Map.toList $ Tx.toTxOut <$> r
      hoistEither $ case filter check refsAndOuts of
        [] -> Left $ WAPI.OtherError "Couldn't find collateral UTxO"
        ((oref, _) : _) -> Right oref
  where
    check (_, txOut) = Tx.txOutValue txOut == collateralValue (cePABConfig cEnv)

{- | Construct a 'NonEmpty' list from a single element.
 Should be replaced by NonEmpty.singleton after updating to base 4.15
-}
nonEmptySingleton :: a -> NonEmpty a
nonEmptySingleton = (:| [])
