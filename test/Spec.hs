module Main (main) where

import Spec.MLabsPAB.Contract qualified as MLabsPAB.Contract
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "MLabsPAB"
    [MLabsPAB.Contract.tests]
