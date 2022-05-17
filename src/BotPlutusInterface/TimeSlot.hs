module BotPlutusInterface.TimeSlot (
  TimeSlotConversionError,
  slotToPOSIXTimeImpl,
  posixTimeToSlotImpl,
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
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF

data TimeSlotConversionError
  = TimeSlotConversionError !Text
  deriving stock (Show)

slotToPOSIXTimeImpl :: PABConfig -> Ledger.Slot -> IO (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeImpl pabConf (Ledger.Slot s) = runEitherT $ do
  let pparams =
        CAPI.toLedgerPParams
          CAPI.ShelleyBasedEraAlonzo -- TODO: should era be passed as an argument?
          (pcProtocolParams pabConf)

  sock <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = NodeInfo (pcNetwork pabConf) sock

  epochInfo <- toLedgerEpochInfo <$> newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo

  let slotNo = CAPI.SlotNo $ fromInteger s
  firstEitherT toError $
    hoistEither $
      slotToPOSIXTime pparams epochInfo sysStart slotNo

toLedgerEpochInfo ::
  CAPI.EraHistory mode ->
  EpochInfo (Either CAPI.TransactionValidityError)
toLedgerEpochInfo (CAPI.EraHistory _ interpreter) =
  hoistEpochInfo (first CAPI.TransactionValidityIntervalError . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

posixTimeToSlotImpl :: PABConfig -> Ledger.POSIXTime -> IO (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlotImpl pabConf pTime = runEitherT $ do
  sock <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = NodeInfo (pcNetwork pabConf) sock

  (CAPI.EraHistory _ interpreter) <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo

  let time :: RelativeTime = toRelativeTime sysStart (toUtc pTime)
      timeQuery = HF.wallclockToSlot time
      int = HF.interpretQuery interpreter timeQuery
  (CAPI.SlotNo s, _, _) <- firstEitherT toError $ hoistEither int

  return $ Ledger.Slot (toInteger s)
  where
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime $
        secondsToNominalDiffTime 
          (fromInteger $ milliseconds `div` 1000) -- FIXME: is it safe?

newET :: Show e => IO (Either e a) -> EitherT TimeSlotConversionError IO a
newET = firstEitherT toError . newEitherT

toError :: Show e => e -> TimeSlotConversionError
toError = TimeSlotConversionError . Text.pack . show
