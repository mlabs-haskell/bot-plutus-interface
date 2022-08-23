module BotPlutusInterface.CardanoNode.Effects (
  utxosAt,
  handleQueryNode,
) where

import BotPlutusInterface.CardanoNode.Query
  (NodeConn,
   NodeQueryError,
   QueryConstraint,
   queryBabbageEra,
   queryInCardanoMode,
   toQueryError
  )


import Cardano.Ledger.Babbage.PParams (_protocolVersion)
import Cardano.Ledger.Alonzo.TxInfo qualified as CApi (slotToPOSIXTime)
import Cardano.Api (LocalNodeConnectInfo (..))
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Control.Lens (folded, to, (^..))
import Control.Monad.Freer (Eff, Members, interpret, send, type (~>), Member)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Address (Address)
import Ledger.Tx.CardanoAPI qualified as TxApi
import Plutus.ChainIndex.Types (ChainIndexTxOut (..))
import Plutus.V2.Ledger.Tx qualified as V2
import BotPlutusInterface.CardanoAPI qualified as CApi
  (addressInEraToAny, fromCardanoTxOut, toCardanoSlotNo, fromCardanoEpochInfo, posixTimeToSlot)
import Prelude

data QueryNode a where
  UtxosAt :: Address -> QueryNode (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
  SlotToPOSIXTime :: Ledger.Slot -> CApi.S.ProtocolParameters -> QueryNode (Either NodeQueryError Ledger.POSIXTime)
  POSIXTimeToSlot :: Ledger.POSIXTime -> QueryNode (Either NodeQueryError Ledger.Slot)
  -- ProtocolParams :: QueryNode (Either NodeQueryError CApi.S.ProtocolParameters)
  -- POSIXTimeRangeToSlotRange :: Ledger.POSIXTimeRange -> QueryNode (Either TimeSlot.TimeSlotConversionError Ledger.SlotRange)

utxosAt ::
  forall effs.
  Member QueryNode effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
utxosAt = send . UtxosAt

slotToPOSIXTime ::
  forall effs.
    Members '[QueryNode, Reader NodeConn] effs =>
    Ledger.Slot ->
    CApi.S.ProtocolParameters ->
    Eff effs (Either NodeQueryError Ledger.POSIXTime)
slotToPOSIXTime slot = send . SlotToPOSIXTime slot

posixTimeToSlot :: 
  forall effs.
    Members '[QueryNode, Reader NodeConn] effs =>
    Ledger.POSIXTime ->
    Eff effs (Either NodeQueryError Ledger.Slot)
posixTimeToSlot = send . POSIXTimeToSlot

handleQueryNode ::
  forall effs.
  QueryConstraint effs =>
  Eff (QueryNode ': effs) ~> Eff effs
handleQueryNode =
  interpret $ \case
    UtxosAt addr -> handleUtxosAt addr
    (SlotToPOSIXTime slot pparams) -> handleSlotToPOSIXTime slot pparams
    (POSIXTimeToSlot pTime) -> handlePOSIXTimeToSlot pTime
    -- ProtocolParams -> handleProtocolParams

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
    CApi.S.ProtocolParameters ->
    Eff effs (Either NodeQueryError Ledger.POSIXTime)
handleSlotToPOSIXTime slot pparams =
  runEitherT $ do

  eraHistory <- newEitherT $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  sysStart   <- newEitherT $ queryInCardanoMode CApi.QuerySystemStart

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

  eraHistory <- newEitherT $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  sysStart   <- newEitherT $ queryInCardanoMode CApi.QuerySystemStart


  firstEitherT toQueryError . hoistEither $
    CApi.posixTimeToSlot sysStart eraHistory pTime
