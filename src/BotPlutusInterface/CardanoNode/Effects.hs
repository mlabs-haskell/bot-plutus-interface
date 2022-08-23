module BotPlutusInterface.CardanoNode.Effects (
  utxosAt,
  handleQueryNode,
) where

import BotPlutusInterface.CardanoNode.Query (
  NodeConn,
  NodeQueryError,
  QueryConstraint,
  queryBabbageEra,
  queryInCardanoMode,
  toQueryError,
 )

import Data.Text (Text)
import Ledger (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )

import BotPlutusInterface.CardanoAPI qualified as CApi (
  addressInEraToAny,
  fromCardanoEpochInfo,
  fromCardanoTxOut,
  posixTimeToSlot,
  toCardanoSlotNo,
 )
import Cardano.Api (LocalNodeConnectInfo (..))
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Cardano.Ledger.Alonzo.TxInfo qualified as CApi (slotToPOSIXTime)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams, _protocolVersion)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Lens (folded, to, (^..))
import Control.Monad.Freer (Eff, Members, interpret, send, type (~>))
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Control.Monad.Trans.Except (throwE)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Address (Address)
import Ledger.Tx.CardanoAPI qualified as TxApi
import Plutus.ChainIndex.Types (ChainIndexTxOut (..))
import Plutus.V2.Ledger.Tx qualified as V2
import Prelude

data QueryNode a where
  UtxosAt :: Address -> QueryNode (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
  SlotToPOSIXTime :: Ledger.Slot -> QueryNode (Either NodeQueryError Ledger.POSIXTime)
  POSIXTimeToSlot :: Ledger.POSIXTime -> QueryNode (Either NodeQueryError Ledger.Slot)
  POSIXTimeRangeToSlotRange :: Ledger.POSIXTimeRange -> QueryNode (Either NodeQueryError Ledger.SlotRange)

utxosAt ::
  forall effs.
  Members '[QueryNode] effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
utxosAt = send . UtxosAt

slotToPOSIXTime ::
  forall effs.
  Members '[QueryNode] effs =>
  Ledger.Slot ->
  Eff effs (Either NodeQueryError Ledger.POSIXTime)
slotToPOSIXTime = send . SlotToPOSIXTime

posixTimeToSlot ::
  forall effs.
  Members '[QueryNode] effs =>
  Ledger.POSIXTime ->
  Eff effs (Either NodeQueryError Ledger.Slot)
posixTimeToSlot = send . POSIXTimeToSlot

posixTimeRangeToSlotRange ::
  forall effs.
  Members '[QueryNode] effs =>
  Ledger.POSIXTimeRange ->
  Eff effs (Either NodeQueryError Ledger.SlotRange)
posixTimeRangeToSlotRange = send . POSIXTimeRangeToSlotRange

handleQueryNode ::
  forall effs.
  QueryConstraint effs =>
  Eff (QueryNode ': effs) ~> Eff effs
handleQueryNode =
  interpret $ \case
    UtxosAt addr -> handleUtxosAt addr
    (SlotToPOSIXTime slot) -> handleSlotToPOSIXTime slot
    (POSIXTimeToSlot pTime) -> handlePOSIXTimeToSlot pTime
    (POSIXTimeRangeToSlotRange pTime) -> handlePOSIXTimeRangeToSlotRange pTime

handleUtxosAt ::
  forall effs.
  QueryConstraint effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
handleUtxosAt addr = runEitherT $ do
  conn <- lift $ ask @NodeConn

  caddr <-
    firstEitherT toQueryError $
      hoistEither $
        TxApi.toCardanoAddressInEra (localNodeNetworkId conn) addr

  let query :: CApi.QueryInShelleyBasedEra era (CApi.UTxO era)
      query = CApi.QueryUTxO $ CApi.QueryUTxOByAddress $ Set.singleton $ CApi.addressInEraToAny caddr

  (CApi.UTxO result) <- newEitherT $ queryBabbageEra query

  chainIndexTxOuts <-
    firstEitherT toQueryError $
      hoistEither $
        sequenceA $
          result ^.. folded . to CApi.fromCardanoTxOut

  let txOutRefs :: [V2.TxOutRef]
      txOutRefs = TxApi.fromCardanoTxIn <$> Map.keys result

  return $ Map.fromList $ zip txOutRefs chainIndexTxOuts

handleSlotToPOSIXTime ::
  forall effs.
  (QueryConstraint effs) =>
  Ledger.Slot ->
  Eff effs (Either NodeQueryError Ledger.POSIXTime)
handleSlotToPOSIXTime slot =
  runEitherT $ do
    eraHistory <- queryEraHistory
    sysStart <- querySystemStart
    pparams <- queryProtocolParams

    let pparams' = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams

        epochInfo = CApi.fromCardanoEpochInfo eraHistory

    firstEitherT toQueryError . hoistEither $
      CApi.slotToPOSIXTime pparams' epochInfo sysStart (CApi.toCardanoSlotNo slot)

handlePOSIXTimeToSlot ::
  forall effs.
  (QueryConstraint effs) =>
  Ledger.POSIXTime ->
  Eff effs (Either NodeQueryError Ledger.Slot)
handlePOSIXTimeToSlot pTime = runEitherT $ do
  eraHistory <- queryEraHistory
  sysStart <- querySystemStart

  firstEitherT toQueryError . hoistEither $
    CApi.posixTimeToSlot sysStart eraHistory pTime

handlePOSIXTimeRangeToSlotRange ::
  forall effs.
  (QueryConstraint effs) =>
  Ledger.POSIXTimeRange ->
  Eff effs (Either NodeQueryError Ledger.SlotRange)
handlePOSIXTimeRangeToSlotRange
  ptr@(Interval (LowerBound start startIncl) (UpperBound end endIncl)) = runEitherT $ do
    sysStart <- querySystemStart
    eraHistory <- queryEraHistory
    pparams' <- queryProtocolParams

    let pparams = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams'
        epochInfo = CApi.fromCardanoEpochInfo eraHistory
        extTimeToExtSlot = convertExtended sysStart eraHistory
        getClosure = getExtClosure pparams epochInfo sysStart

    -- -- conversions
    startSlot <- extTimeToExtSlot start
    startSlotClosure <- getClosure startSlot startIncl
    endSlot <- extTimeToExtSlot end
    endSlotClosure <- getClosure endSlot endIncl
    --
    let lowerB :: LowerBound Ledger.Slot
        lowerB = LowerBound startSlot startSlotClosure

        upperB :: UpperBound Ledger.Slot
        upperB = UpperBound endSlot endSlotClosure

        range :: Ledger.SlotRange
        range = Interval lowerB upperB

    pure range
    where
      -- helper to convert `Extended POSIXTime` to `Extended Slot`
      -- using `posixTimeToSlot`
      convertExtended ::
        Monad m =>
        SystemStart ->
        CApi.EraHistory CApi.CardanoMode ->
        Extended Ledger.POSIXTime ->
        EitherT NodeQueryError m (Extended Ledger.Slot)
      convertExtended sysStart eraHist =
        firstEitherT toQueryError . hoistEither . \case
          Finite pTime -> Finite <$> CApi.posixTimeToSlot sysStart eraHist pTime
          NegInf -> pure NegInf
          PosInf -> pure PosInf

      -- helper to calculate bound's closure
      -- if bound is not `NegInf` or `PosInf`, then `Closure` need to be calculated
      -- https://github.com/input-output-hk/plutus-apps/blob/e51f57fa99f4cc0942ba6476b0689e43f0948eb3/plutus-ledger/src/Ledger/TimeSlot.hs#L125-L130
      getExtClosure ::
        Monad m =>
        PParams (BabbageEra StandardCrypto) ->
        EpochInfo (Either Text) ->
        SystemStart ->
        Extended Ledger.Slot ->
        Bool -> -- current `Closure` of lower or upper bound of `Ledger.POSIXTimeRange`
        EitherT NodeQueryError m Bool
      getExtClosure pparams epochInfo sysStart exSlot currentClosure =
        firstEitherT toQueryError . hoistEither $
          case exSlot of
            Finite slot -> do
              slotsTime <- CApi.slotToPOSIXTime pparams epochInfo sysStart (CApi.toCardanoSlotNo slot)
              pure $ slotsTime `Ledger.member` ptr
            NegInf -> pure currentClosure
            PosInf -> pure currentClosure

-- Helpers
queryEraHistory ::
  forall effs.
  QueryConstraint effs =>
  EitherT NodeQueryError (Eff effs) (CApi.EraHistory CApi.CardanoMode)
queryEraHistory = newEitherT $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)

querySystemStart ::
  forall effs.
  QueryConstraint effs =>
  EitherT NodeQueryError (Eff effs) SystemStart
querySystemStart = newEitherT $ queryInCardanoMode CApi.QuerySystemStart

queryProtocolParams ::
  forall effs.
  QueryConstraint effs =>
  EitherT NodeQueryError (Eff effs) CApi.S.ProtocolParameters
queryProtocolParams = do
  let query =
        CApi.QueryInEra CApi.BabbageEraInCardanoMode $
          CApi.QueryInShelleyBasedEra CApi.ShelleyBasedEraBabbage CApi.QueryProtocolParameters

  result <- newEitherT $ queryInCardanoMode query

  case result of
    Left err -> throwE $ toQueryError err
    Right pparams -> return pparams
