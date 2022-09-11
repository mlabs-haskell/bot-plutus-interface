{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface (runPAB) where

import BotPlutusInterface.Server qualified as Server
import BotPlutusInterface.Types (HasContract, PABConfig (..))
import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Network.Wai.Handler.Warp (run)
import Prelude

runPAB :: forall (t :: Type). (HasContract t, FromJSON t) => PABConfig -> IO ()
runPAB pabConf = do
  putStrLn "Starting BotPlutusInterface server"
  state <- Server.initState

  run (pcPort pabConf) (Server.app @t pabConf state)
