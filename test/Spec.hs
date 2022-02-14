module Main (main) where

import Spec.BotPlutusInterface.Balance qualified
import Spec.BotPlutusInterface.Contract qualified
import Spec.BotPlutusInterface.UtxoParser qualified
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
    "BotPlutusInterface"
    [ Spec.BotPlutusInterface.Contract.tests
    , Spec.BotPlutusInterface.UtxoParser.tests
    , Spec.BotPlutusInterface.Balance.tests
    ]
