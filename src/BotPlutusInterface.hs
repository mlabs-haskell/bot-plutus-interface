{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface (runPAB) where

import BotPlutusInterface.Server qualified as Server
import BotPlutusInterface.Types (PABConfig (..))
import Data.Aeson (FromJSON)
import Network.Wai.Handler.Warp (run)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions)
import Relude hiding (state)

runPAB :: forall (t :: Type). (HasDefinitions t, FromJSON t) => PABConfig -> IO ()
runPAB pabConf = do
  putStrLn "Starting BotPlutusInterface server"
  state <- Server.initState

  run pabConf.pcPort (Server.app @t pabConf state)
