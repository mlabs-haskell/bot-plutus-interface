module BotPlutusInterface.Helpers (awaitTxConfirmedUntilSlot) where

import Control.Lens (review)
import Data.Text (pack)
import Ledger (Slot, TxId)
import Plutus.Contract.Error (AsContractError, _OtherContractError)
import Plutus.Contract.Request (RollbackState (Unknown), awaitTxStatusChange, currentSlot, waitNSlots)
import Plutus.Contract.Types (Contract, throwError)
import Prelude

awaitTxConfirmedUntilSlot :: forall w s e. (AsContractError e) => TxId -> Slot -> Contract w s e ()
awaitTxConfirmedUntilSlot txId maxSlot = go 0
  where
    go :: Integer -> Contract w s e ()
    go n = do
      mTx <- awaitTxStatusChange txId
      case mTx of
        Unknown -> do
          curSlot <- currentSlot
          if curSlot > maxSlot
            then
              throwError @e $
                review _OtherContractError $
                  pack $
                    "Could not find transaction - " ++ show txId ++ " - before slot " ++ show maxSlot
            else do
              _ <- waitNSlots 20
              go (n + 1)
        _ -> pure ()
