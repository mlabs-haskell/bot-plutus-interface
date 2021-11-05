{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module MLabsPAB (runPAB) where

import Data.Aeson (FromJSON)
import MLabsPAB.Server qualified as Server
import MLabsPAB.Types (PABConfig (..))
import Network.Wai.Handler.Warp (run)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions)
import Prelude

runPAB :: forall t. (HasDefinitions t, FromJSON t) => PABConfig -> IO ()
runPAB pabConf = do
  putStrLn "Starting MLabsPAB server"
  state <- Server.initState

  run 9080 (Server.app @t pabConf state)
