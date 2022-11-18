{- | This module provides conversions between `Slot` and `POSIXTime`.
 It uses node queries to get additional parameters and emulate conversions that happen in the ledger
 when it builds `TxInfo`. It aims to provide reliable conversion, enabling execution of contracts
 with exact time intervals validation, and should not be affected by slot length changes unlike
 current plutus-ledger conversion functions.
-}
module BotPlutusInterface.TimeSlot (
  TimeSlotConversionError,
  slotToPOSIXTimeIO,
  posixTimeToSlotIO,
  posixTimeRangeToContainedSlotRangeIO,
  posixTimeToSlotLengthIO,
) where

import BotPlutusInterface.CardanoNode.Query (
  QueryConstraint,
  connectionInfo,
  queryBabbageEra,
  queryInCardanoMode,
 )
import BotPlutusInterface.Types (
  PABConfig,
 )
import Cardano.Api (CardanoMode, EraHistory)
import Cardano.Api qualified as CApi
import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Reader (runReader)

import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams, _protocolVersion)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart, getSlotLength, toRelativeTime)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Either (
  EitherT,
  bimapEitherT,
  firstEitherT,
  hoistEither,
  newEitherT,
  runEitherT,
 )
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ledger (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Ledger qualified
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF
import Prelude

-- | Error returned by the functions of this module
data TimeSlotConversionError
  = TimeSlotConversionError !Text
  deriving stock (Show)

-- Analogous to `slotToBeginPOSIXTime` from plutus-ledger

-- | Convert `Slot` to `POSIXTime`.
slotToPOSIXTimeIO :: PABConfig -> Ledger.Slot -> IO (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeIO pabConf slot = do
  conn <- connectionInfo pabConf
  runM $ runReader conn (slotToPOSIXTimeIO' slot)

slotToPOSIXTimeIO' ::
  QueryConstraint effs =>
  Ledger.Slot ->
  Eff effs (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeIO' slot = runEitherT $ do
  sysStart <- newET $ queryInCardanoMode CApi.QuerySystemStart
  eraHistory <- newET $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  pparams <- newET $ queryBabbageEra CApi.QueryProtocolParameters
  let epochInfo = toLedgerEpochInfo eraHistory
      pparamsInEra = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams

  firstEitherT toError . hoistEither $
    slotToPOSIXTime pparamsInEra epochInfo sysStart (toSlotNo slot)

-- | Convert `POSIXTime` to `Slot`.
posixTimeToSlotIO :: PABConfig -> Ledger.POSIXTime -> IO (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlotIO pabConf pTime = do
  conn <- connectionInfo pabConf
  runM $ runReader conn (posixTimeToSlot' pTime)

posixTimeToSlot' ::
  QueryConstraint effs =>
  Ledger.POSIXTime ->
  Eff effs (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlot' pTime = runEitherT $ do
  sysStart <- newET $ queryInCardanoMode CApi.QuerySystemStart
  eraHistory <- newET $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  firstEitherT toError . hoistEither $
    posixTimeToSlot sysStart eraHistory pTime

posixTimeRangeToContainedSlotRangeIO ::
  PABConfig -> Ledger.POSIXTimeRange -> IO (Either TimeSlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRangeIO pabConf ptr = do
  conn <- connectionInfo pabConf
  runM $ runReader conn (posixTimeRangeToContainedSlotRange' ptr)

{- | Convert a `POSIXTimeRange` to `SlotRange`.
 Gives the biggest slot range that is entirely contained by the given time range.
-}

-- Analogous to `posixTimeRangeToContainedSlotRange` from plutus-ledger
posixTimeRangeToContainedSlotRange' ::
  QueryConstraint effs =>
  Ledger.POSIXTimeRange ->
  Eff effs (Either TimeSlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRange'
  ptr@(Interval (LowerBound start startIncl) (UpperBound end endIncl)) = runEitherT $ do
    -- getting required info from node

    sysStart <- newET $ queryInCardanoMode CApi.QuerySystemStart
    eraHistory <- newET $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)

    let epochInfo = toLedgerEpochInfo eraHistory

    pparams <- newET $ queryBabbageEra CApi.QueryProtocolParameters

    let pparamsInEra = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage pparams
        extTimeToExtSlot = convertExtended sysStart eraHistory
        getClosure = getExtClosure pparamsInEra epochInfo sysStart

    -- conversions
    startSlot <- extTimeToExtSlot start
    startSlotClosure <- getClosure startSlot startIncl
    endSlot <- extTimeToExtSlot end
    endSlotClosure <- getClosure endSlot endIncl

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
        EraHistory CardanoMode ->
        Extended Ledger.POSIXTime ->
        EitherT TimeSlotConversionError m (Extended Ledger.Slot)
      convertExtended sysStart eraHist =
        firstEitherT toError . hoistEither . \case
          Finite pTime -> Finite <$> posixTimeToSlot sysStart eraHist pTime
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
        EitherT TimeSlotConversionError m Bool
      getExtClosure pparams epochInfo sysStart exSlot currentClosure =
        firstEitherT toError . hoistEither $
          case exSlot of
            Finite slot -> do
              slotsTime <- slotToPOSIXTime pparams epochInfo sysStart (toSlotNo slot)
              pure $ slotsTime `Ledger.member` ptr
            NegInf -> pure currentClosure
            PosInf -> pure currentClosure

-- | Convert `POSIXTime` to `Slot` using `SystemStart` and `EraHistory`.
posixTimeToSlot ::
  SystemStart ->
  EraHistory CardanoMode ->
  Ledger.POSIXTime ->
  Either HF.PastHorizonException Ledger.Slot
posixTimeToSlot sysStart eraHist pTime = do
  -- toRelativeTime checks that pTime >= sysStart via `Control.Exception.assert`
  let relativeTime = toRelativeTime sysStart (toUtc pTime)
      (CApi.EraHistory _ int) = eraHist
      query = HF.wallclockToSlot relativeTime

  (sn, _, _) <- HF.interpretQuery int query
  pure (fromSlotNo sn)
  where
    toUtc :: Ledger.POSIXTime -> UTCTime
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime
        . secondsToNominalDiffTime
        $ fromInteger milliseconds / 1000

-- helper functions --

-- | Ledger Slot to "Cardano.Api" Slot conversion
toSlotNo :: Ledger.Slot -> CApi.SlotNo
toSlotNo (Ledger.Slot s) = CApi.SlotNo $ fromInteger s

-- | Cardano.Api Slot to Ledger Slot conversion
fromSlotNo :: CApi.SlotNo -> Ledger.Slot
fromSlotNo (CApi.SlotNo s) = Ledger.Slot (toInteger s)

-- helpler to lift IO to EitherT with desired `TimeSlotConversionError` error type
newET :: (Show e, Monad m) => m (Either e a) -> EitherT TimeSlotConversionError m a
newET = firstEitherT toError . newEitherT

toError :: Show e => e -> TimeSlotConversionError
toError = TimeSlotConversionError . Text.pack . show

-- "Ported" from cardano-node:
-- https://github.com/input-output-hk/cardano-node/blob/64f3d19a681a872f07bd61e6cc473738e78ab0e8/cardano-api/src/Cardano/Api/Fees.hs#L560

-- | Get Ledger `EpochInfo` from "Cardano.Api" `EraHistory`.
toLedgerEpochInfo ::
  CApi.EraHistory mode ->
  EpochInfo (Either Text)
toLedgerEpochInfo (CApi.EraHistory _ interpreter) =
  hoistEpochInfo (first (Text.pack . show) . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

posixTimeToSlotLengthIO :: PABConfig -> Ledger.POSIXTime -> IO (Either TimeSlotConversionError NominalDiffTime)
posixTimeToSlotLengthIO pabConf time = do
  conn <- connectionInfo pabConf
  runM $ runReader conn (posixTimeToSlotLengthIO' time)

posixTimeToSlotLengthIO' ::
  QueryConstraint effs =>
  Ledger.POSIXTime ->
  Eff effs (Either TimeSlotConversionError NominalDiffTime)
posixTimeToSlotLengthIO' time = runEitherT $ do
  sysStart <- newET $ queryInCardanoMode CApi.QuerySystemStart
  eraHistory <- newET $ queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)
  slot <- firstEitherT toError . hoistEither $ posixTimeToSlot sysStart eraHistory time

  let query = HF.slotToSlotLength $ toSlotNo slot
      (CApi.EraHistory _ int) = eraHistory
  bimapEitherT toError getSlotLength $ hoistEither $ HF.interpretQuery int query
