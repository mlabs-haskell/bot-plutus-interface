module MLabsPAB (runPAB) where

import Data.Aeson (FromJSON)
import Data.Proxy (Proxy)
import MLabsPAB.Server qualified as Server
import MLabsPAB.Types (PABConfig (..))
import Network.Wai.Handler.Warp (run)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions)
import Prelude

runPAB :: (HasDefinitions t, FromJSON t) => PABConfig -> Proxy t -> IO ()
runPAB pabConf contractDef = do
  putStrLn "Starting MLabsPAB server"
  state <- Server.initState

  run 9080 (Server.app pabConf contractDef state)
