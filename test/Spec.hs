module Main (main) where

import Spec.BotPlutusInterface.Balance qualified
import Spec.BotPlutusInterface.Collateral qualified
import Spec.BotPlutusInterface.Contract qualified
import Spec.BotPlutusInterface.ContractStats qualified
import Spec.BotPlutusInterface.Server qualified
import Spec.BotPlutusInterface.TxStatusChange qualified
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
    , Spec.BotPlutusInterface.Server.tests
    , Spec.BotPlutusInterface.ContractStats.tests
    , Spec.BotPlutusInterface.TxStatusChange.tests
    , Spec.BotPlutusInterface.Collateral.tests
    ]
