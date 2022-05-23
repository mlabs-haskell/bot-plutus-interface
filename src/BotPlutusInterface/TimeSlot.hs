module BotPlutusInterface.TimeSlot (
  TimeSlotConversionError,
  slotToPOSIXTimeImpl,
  posixTimeToSlotImpl,
  posixTimeRangeToContainedSlotRangeImpl,
) where

import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Ledger qualified
import Prelude

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo), queryEraHistory, querySystemStart)
import BotPlutusInterface.Types (PABConfig, pcNetwork, pcProtocolParams)
import Cardano.Api qualified as CAPI
import Cardano.Ledger.Alonzo.PParams (_protocolVersion)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import System.Environment (getEnv)

import Cardano.Slotting.Time (RelativeTime, toRelativeTime)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ledger (Closure, Extended (Finite, NegInf, PosInf), Interval (Interval), LowerBound (LowerBound), UpperBound (UpperBound))
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF

data TimeSlotConversionError
  = TimeSlotConversionError !Text
  deriving stock (Show)

slotToPOSIXTimeImpl :: PABConfig -> Ledger.Slot -> IO (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeImpl pabConf slot = runEitherT (slotToPOSIXTimeImpl' pabConf slot)

slotToPOSIXTimeImpl' :: PABConfig -> Ledger.Slot -> EitherT TimeSlotConversionError IO  Ledger.POSIXTime
slotToPOSIXTimeImpl' pabConf (Ledger.Slot s) = do
  nodeInfo <- NodeInfo (pcNetwork pabConf) <$>
                liftIO (getEnv "CARDANO_NODE_SOCKET_PATH")
  eraHsitory <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo

  let slotNo = CAPI.SlotNo $ fromInteger s
      epochInfo = toLedgerEpochInfo eraHsitory
      pparams = 
        CAPI.toLedgerPParams
          CAPI.ShelleyBasedEraAlonzo
          (pcProtocolParams pabConf)

  -- TODO: doc about diff in ToEnd conversions
  firstEitherT toError $
    hoistEither $
      slotToPOSIXTime pparams epochInfo sysStart slotNo

-- return $ case toWhichTime of
--   ToBeginTime -> resultTime
--   ToEndTime -> resultTime -- + Ledger.POSIXTime (slotLengthToMillisec slotLength - 1)
  where
    toLedgerEpochInfo ::
      CAPI.EraHistory mode ->
      EpochInfo (Either CAPI.TransactionValidityError)
    toLedgerEpochInfo (CAPI.EraHistory _ interpreter) =
      hoistEpochInfo (first CAPI.TransactionValidityIntervalError . runExcept) $
        Consensus.interpreterToEpochInfo interpreter

posixTimeToSlotImpl :: 
  PABConfig -> 
  Ledger.POSIXTime -> 
  IO (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlotImpl pabConf pTime = runEitherT (posixTimeToSlotImpl' pabConf pTime)

posixTimeToSlotImpl' :: 
  PABConfig -> 
  Ledger.POSIXTime -> 
  EitherT TimeSlotConversionError IO Ledger.Slot
posixTimeToSlotImpl' pabConf pTime = do
  sock <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = NodeInfo (pcNetwork pabConf) sock

  (CAPI.EraHistory _ interpreter) <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo

  let time :: RelativeTime = toRelativeTime sysStart (toUtc pTime)
      timeQuery = HF.wallclockToSlot time
  (CAPI.SlotNo s, _, _) <- firstEitherT toError $ 
                            hoistEither (HF.interpretQuery interpreter timeQuery)

  return $ Ledger.Slot (toInteger s)
  where
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime $
        secondsToNominalDiffTime
          (fromInteger $ milliseconds `div` 1000)

posixTimeRangeToContainedSlotRangeImpl ::
  PABConfig ->
  Ledger.POSIXTimeRange ->
  IO (Either TimeSlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRangeImpl
  pabConf
  ptr@(Interval (LowerBound start startIncl) (UpperBound end endIncl)) = runEitherT $ do
    startSlot <- convertExtended start
    startSlotClosure <- getClosure startSlot startIncl

    endSlot <- convertExtended end
    endSlotClosure <- getClosure endSlot endIncl

    let lowerB = LowerBound startSlot startSlotClosure
        upperB = UpperBound endSlot endSlotClosure

    pure $ Interval lowerB upperB
    where
      convertExtended :: Extended Ledger.POSIXTime -> EitherT TimeSlotConversionError IO (Extended Ledger.Slot)
      convertExtended = \case
        Finite pTime -> Finite <$> posixTimeToSlotImpl' pabConf pTime
        NegInf -> pure NegInf
        PosInf -> pure PosInf

      getClosure :: Extended Ledger.Slot -> Closure -> EitherT TimeSlotConversionError IO Bool
      getClosure exSlot currentClosure = case exSlot of
        Finite slot -> do
          slotsTime <- slotToPOSIXTimeImpl' pabConf slot
          pure $ slotsTime `Ledger.member` ptr
        NegInf -> pure currentClosure
        PosInf -> pure currentClosure

-- helper functions

newET :: Show e => IO (Either e a) -> EitherT TimeSlotConversionError IO a
newET = firstEitherT toError . newEitherT

toError :: Show e => e -> TimeSlotConversionError
toError = TimeSlotConversionError . Text.pack . show
