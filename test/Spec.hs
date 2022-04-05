module Main (main) where

import Spec.BotPlutusInterface.Balance qualified
import Spec.BotPlutusInterface.Config qualified
import Spec.BotPlutusInterface.Contract qualified
import Spec.BotPlutusInterface.Server qualified
import Spec.BotPlutusInterface.UtxoParser qualified
import Spec.PlutusConfig.Base qualified
import Spec.PlutusConfig.Cardano.Api qualified
import Spec.PlutusConfig.Cardano.Api.Shelley qualified
import Spec.PlutusConfig.Ledger qualified
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
    [ Spec.BotPlutusInterface.Balance.tests
    , Spec.BotPlutusInterface.Config.tests
    , Spec.BotPlutusInterface.Contract.tests
    , Spec.BotPlutusInterface.Server.tests
    , Spec.BotPlutusInterface.UtxoParser.tests
    , Spec.PlutusConfig.Base.tests
    , Spec.PlutusConfig.Cardano.Api.Shelley.tests
    , Spec.PlutusConfig.Cardano.Api.tests
    , Spec.PlutusConfig.Ledger.tests
    ]
