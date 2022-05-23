module Main (main) where

import PlutipRun qualified as Plutip
import TestnetRun qualified as Testnet
import Prelude

{- | For running fast live tests using Plutip's local cluster,
 needed only for debugging period
-}
main :: IO ()
main = do
  -- Plutip.plutipRun
  Testnet.testnetRun
