{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.PlutusExample.Transfer (
  TransferParams (TransferParams),
  TransferSchema,
  transfer,
) where

import Control.Monad (forM_)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bifunctor (first)
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Plutus.Contract (Contract, Endpoint, submitTx, tell, waitNSlots)
import Schema (ToSchema)
import Prelude

type TransferSchema =
  Endpoint "transfer" TransferParams

data TransferParams = TransferParams
  { tfpOutputPerTx :: Int
  , tfpPayments :: [(PubKeyHash, Value)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''TransferParams)

transfer :: TransferParams -> Contract (Last Text) TransferSchema Text ()
transfer (TransferParams outputPerTx allPayments) = do
  tell $ Last $ Just "Contract started"
  let txs =
        map toTx $ group outputPerTx allPayments
  forM_ txs $ \tx -> submitTx tx >> waitNSlots 1
  tell $ Last $ Just "Finished"
  where
    toTx :: [(PubKeyHash, Value)] -> TxConstraints Void Void
    toTx = mconcat . map (uncurry Constraints.mustPayToPubKey . first PaymentPubKeyHash)

group :: Int -> [a] -> [[a]]
group n list
  | length list <= n = [list]
  | otherwise = let (xs, xss) = splitAt n list in xs : group n xss
