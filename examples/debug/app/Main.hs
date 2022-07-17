module Main (main) where

import TestRun qualified
import Prelude

{- | For running fast live tests using Plutip's local cluster,
 needed only for debugging period
-}
main :: IO ()
main = do
  TestRun.testnetRun

