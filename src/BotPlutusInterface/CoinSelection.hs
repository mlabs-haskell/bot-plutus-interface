module BotPlutusInterface.CoinSelection () where

import Control.Lens (_3, (^.))
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)

import Ledger.Value qualified as Value
import Ledger.Value (Value)



import Prelude

valueToVec :: Value -> Seq Integer
valueToVec = Seq.fromList . map (^. _3) . Value.flattenValue

l2norm :: Seq Integer -> Seq Integer -> Float
l2norm v1 v2 = sqrt $ fromInteger $ sum $ Seq.zipWith (\n1 n2 -> (n1 - n2)^2) v1 v2
