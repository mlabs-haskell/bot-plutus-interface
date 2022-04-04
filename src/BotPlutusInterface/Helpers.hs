module BotPlutusInterface.Helpers (awaitTxConfirmedUntilSlot) where

import Control.Lens (review)
import Data.Text (pack)
import Ledger (Slot, TxId)
import Plutus.Contract.Error (AsContractError, _OtherContractError)
import Plutus.Contract.Request (RollbackState (Unknown), awaitTxStatusChange, currentSlot, waitNSlots)
import Plutus.Contract.Types (Contract, throwError)
import Prelude

awaitTxConfirmedUntilSlot :: forall w s e. (AsContractError e) => TxId -> Slot -> Contract w s e ()
awaitTxConfirmedUntilSlot i maxSlot = go 0
  where
    go :: Integer -> Contract w s e ()
    go n = do
      mTx <- awaitTxStatusChange i
      case mTx of
        Unknown -> do
          curSlot <- currentSlot
          if curSlot >= maxSlot
            then throwError @e $ review _OtherContractError $ pack $ "Could not find tx after maxAttempts. ID: " ++ show i
            else do
              _ <- waitNSlots 20
              go (n + 1)
        _ -> pure ()
