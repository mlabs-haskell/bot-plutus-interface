module BotPlutusInterface.Helpers (awaitTxConfirmedUntilTime) where

import Control.Lens (review)
import Control.Monad (void)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (pack)
import Ledger (POSIXTime, TxId)
import Plutus.Contract.Error (AsContractError, _OtherContractError)
import Plutus.Contract.Request (RollbackState (Unknown), awaitTxStatusChange, currentTime, waitNSlots)
import Plutus.Contract.Types (Contract, throwError)
import Prelude

awaitTxConfirmedUntilTime :: forall (w :: Type) (s :: Row Type) (e :: Type). (AsContractError e) => TxId -> POSIXTime -> Contract w s e ()
awaitTxConfirmedUntilTime txId maxTime = do
  mTx <- awaitTxStatusChange txId
  case mTx of
    Unknown -> do
      curTime <- currentTime
      if curTime > maxTime
        then
          throwError $
            review _OtherContractError $
              pack $
                "Could not find transaction - " ++ show txId ++ " - before " ++ show maxTime
        else do
          void $ waitNSlots 20
          awaitTxConfirmedUntilTime txId maxTime
    _ -> pure ()
