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
) where

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo), queryEraHistory, querySystemStart)
import BotPlutusInterface.Types (PABConfig, pcNetwork, pcProtocolParams)
import Cardano.Api (CardanoMode, EraHistory)
import Cardano.Api qualified as CAPI
import Cardano.Ledger.Alonzo.PParams (_protocolVersion)
import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart, toRelativeTime)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ledger (Extended (Finite, NegInf, PosInf), Interval (Interval), LowerBound (LowerBound), UpperBound (UpperBound))
import Ledger qualified
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF
import System.Environment (getEnv)
import Prelude

-- | Error returned by the functions of this module
data TimeSlotConversionError
  = TimeSlotConversionError !Text
  deriving stock (Show)

-- Analogous to `slotToBeginPOSIXTime` from plutus-ledger

-- | Convert `Slot` to `POSIXTime`.
slotToPOSIXTimeIO :: PABConfig -> Ledger.Slot -> IO (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeIO pabConf lSlot = runEitherT $ do
  nodeInfo <- liftIO $ mkNodeInfo pabConf
  eraHsitory <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo
  let epochInfo = toLedgerEpochInfo eraHsitory
      pparams =
        CAPI.toLedgerPParams
          CAPI.ShelleyBasedEraAlonzo
          (pcProtocolParams pabConf)

  firstEitherT toError . hoistEither $
    slotToPOSIXTime pparams epochInfo sysStart (toSlotNo lSlot)

-- | Convert `POSIXTime` to `Slot`.

-- Analogous to `posixTimeToEnclosingSlot` from plutus-ledger
posixTimeToSlotIO ::
  PABConfig ->
  Ledger.POSIXTime ->
  IO (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlotIO pabConf pTime = runEitherT $ do
  nodeInfo <- liftIO $ mkNodeInfo pabConf
  eraHist <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo
  firstEitherT toError . hoistEither $
    posixTimeToSlot sysStart eraHist pTime

{- | Convert a `POSIXTimeRange` to `SlotRange`.
 Gives the biggest slot range that is entirely contained by the given time range.
-}

-- Analogous to `posixTimeRangeToContainedSlotRange` from plutus-ledger
posixTimeRangeToContainedSlotRangeIO ::
  PABConfig ->
  Ledger.POSIXTimeRange ->
  IO (Either TimeSlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRangeIO
  pabConf
  ptr@(Interval (LowerBound start startIncl) (UpperBound end endIncl)) = runEitherT $ do
    -- getting required info from node
    nodeInfo <- liftIO $ mkNodeInfo pabConf
    sysStart <- newET $ querySystemStart nodeInfo
    eraHsitory <- newET (queryEraHistory nodeInfo)
    let epochInfo = toLedgerEpochInfo eraHsitory
        pparams =
          CAPI.toLedgerPParams
            CAPI.ShelleyBasedEraAlonzo
            (pcProtocolParams pabConf)

    let extTimeToExtSlot = convertExtended sysStart eraHsitory
        getClosure = getExtClosure pparams epochInfo sysStart

    -- conversions
    startSlot <- extTimeToExtSlot start
    startSlotClosure <- getClosure startSlot startIncl
    endSlot <- extTimeToExtSlot end
    endSlotClosure <- getClosure endSlot endIncl
    let lowerB = LowerBound startSlot startSlotClosure
        upperB = UpperBound endSlot endSlotClosure
    pure $ Interval lowerB upperB
    where
      convertExtended sysStart eraHist =
        firstEitherT toError . hoistEither . \case
          Finite pTime -> do
            s <- posixTimeToSlot sysStart eraHist pTime
            pure . Finite . Ledger.Slot . toInteger $ s
          NegInf -> pure NegInf
          PosInf -> pure PosInf

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
posixTimeToSlot sysStart eraHist pTime =
  -- toRelativeTime checks that pTime >= sysStart via `Control.Exception.assert`
  let relativeTime = toRelativeTime sysStart (toUtc pTime)
      (CAPI.EraHistory _ int) = eraHist
   in HF.interpretQuery int (HF.wallclockToSlot relativeTime)
        >>= \(s, _, _) -> return (fromSlotNo s)
  where
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime
        . secondsToNominalDiffTime
        $ fromInteger (milliseconds `div` 1000)

-- helper functions --

-- | Ledger Slot to "Cardano.Api" Slot conversion
toSlotNo :: Ledger.Slot -> CAPI.SlotNo
toSlotNo (Ledger.Slot s) = CAPI.SlotNo $ fromInteger s

-- | Cardano.Api Slot to Ledger Slot conversion
fromSlotNo :: CAPI.SlotNo -> Ledger.Slot
fromSlotNo (CAPI.SlotNo s) = Ledger.Slot (toInteger s)

-- helpler to lift IO to EitherT with desired `TimeSlotConversionError` error type
newET :: Show e => IO (Either e a) -> EitherT TimeSlotConversionError IO a
newET = firstEitherT toError . newEitherT

toError :: Show e => e -> TimeSlotConversionError
toError = TimeSlotConversionError . Text.pack . show

-- "Ported" from cardano-node:
-- https://github.com/input-output-hk/cardano-node/blob/64f3d19a681a872f07bd61e6cc473738e78ab0e8/cardano-api/src/Cardano/Api/Fees.hs#L560

-- | Get Ledger `EpochInfo` from "Cardano.Api" `EraHistory`.
toLedgerEpochInfo ::
  CAPI.EraHistory mode ->
  EpochInfo (Either CAPI.TransactionValidityError)
toLedgerEpochInfo (CAPI.EraHistory _ interpreter) =
  hoistEpochInfo (first CAPI.TransactionValidityIntervalError . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

mkNodeInfo :: PABConfig -> IO NodeInfo
mkNodeInfo pabConf =
  NodeInfo (pcNetwork pabConf)
    <$> getEnv "CARDANO_NODE_SOCKET_PATH"
