module Main (main) where

import Spec.MLabsPAB.Contract qualified
import Spec.MLabsPAB.PreBalance qualified
import Spec.MLabsPAB.UtxoParser qualified
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
    [ Spec.MLabsPAB.Contract.tests
    , Spec.MLabsPAB.UtxoParser.tests
    , Spec.MLabsPAB.PreBalance.tests
    ]
