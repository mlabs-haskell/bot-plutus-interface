{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface (runPAB) where

import BotPlutusInterface.Server qualified as Server
import BotPlutusInterface.Types (PABConfig (pcPort))
import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Network.Wai.Handler.Warp (run)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions)
import Prelude

runPAB :: forall (t :: Type). (HasDefinitions t, FromJSON t) => PABConfig -> IO ()
runPAB pabConf = do
  putStrLn "Starting BotPlutusInterface server"
  state <- Server.initState

  run (pcPort pabConf) (Server.app @t pabConf state)
