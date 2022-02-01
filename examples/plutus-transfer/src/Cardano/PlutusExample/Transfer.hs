module Cardano.PlutusExample.Transfer (TransferSchema, transfer) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Plutus.Contract (Contract, Endpoint, submitTx, tell)
import PlutusTx.Prelude hiding (Semigroup (..), unless)

type TransferSchema =
  Endpoint "transfer" [(PubKeyHash, Value)]

transfer :: [(PubKeyHash, Value)] -> Contract (Last Text) TransferSchema Text ()
transfer payments = do
  tell $ Last $ Just "Contract started"
  let tx = mconcat $ map (uncurry Constraints.mustPayToPubKey . first PaymentPubKeyHash) payments
  void $ submitTx tx
  tell $ Last $ Just "Finished"
