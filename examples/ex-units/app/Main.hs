module Main (main) where

import BotPlutusInterface.Contract qualified as BPI
import BotPlutusInterface.QueryNode qualified as BPI
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Mainnet))
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Concurrent.STM (newTVarIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as JSON
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Ledger (PubKeyHash)
import LockSpend (lockThenSpend)

-- import LockSpendSingle (lockThenSpendSingle)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (listDirectory)
import System.Environment (getArgs, getEnv, setEnv)
import System.FilePath ((</>))
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude

{- | For running fast live tests using Plutip's local cluster,
 needed only for debugging period
-}
main :: IO ()
main = do
  [sockPath, clusterDir, cliDir] <- getArgs
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
  getEnv "PATH" >>= \p -> setEnv "PATH" (p ++ ":" ++ cliDir)
  let nodeInfo = BPI.NodeInfo Mainnet sockPath

  cEnv <- mkContractEnv nodeInfo clusterDir
  res <- BPI.runContract cEnv lockThenSpend
  putStrLn $ case res of
    Right _ -> "=== OK ==="
    Left e -> "=== FAILED ===\n" ++ show e

mkContractEnv :: Monoid w => BPI.NodeInfo -> FilePath -> IO (ContractEnvironment w)
mkContractEnv nodeInfo clusterDir = do
  (pparams, paramsFile) <- getPparams nodeInfo clusterDir
  contractInstanceID <- ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  pkhs <- getPkhs clusterDir
  return $
    ContractEnvironment
      { cePABConfig = mkPabConf pparams (Text.pack paramsFile) clusterDir (head pkhs)
      , ceContractState = contractState
      , ceContractInstanceId = contractInstanceID
      }

getPparams :: BPI.NodeInfo -> FilePath -> IO (ProtocolParameters, FilePath)
getPparams nodeInfo clusterDir = do
  pparams :: ProtocolParameters <- getOrFailM $ BPI.queryProtocolParams nodeInfo
  let ppath = clusterDir </> "pparams.json"
  JSON.encodeFile ppath pparams
  return (pparams, ppath)

mkPabConf :: ProtocolParameters -> Text -> FilePath -> PubKeyHash -> PABConfig
mkPabConf pparams pparamsFile clusterDir ownPkh =
  PABConfig
    { pcCliLocation = Local
    , pcNetwork = Mainnet
    , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
    , pcPort = 9080
    , pcProtocolParams = pparams
    , pcTipPollingInterval = 1_000_000
    , pcSlotConfig = def
    , pcOwnPubKeyHash = ownPkh
    , pcScriptFileDir = Text.pack $ clusterDir </> "bot-plutus-interface/scripts"
    , pcSigningKeyFileDir = Text.pack $ clusterDir </> "bot-plutus-interface/signing-keys"
    , pcTxFileDir = Text.pack $ clusterDir </> "bot-plutus-interface/txs"
    , pcDryRun = False
    , pcLogLevel = Error
    , pcProtocolParamsFile = pparamsFile
    , pcEnableTxEndpoint = True
    }

getPkhs :: FilePath -> IO [PubKeyHash]
getPkhs bpiDir = do
  let dir = bpiDir </> "bot-plutus-interface/signing-keys"
      replace =
        Text.unpack
          . Text.replace "signing-key-" ""
          . Text.replace ".skey" ""
          . Text.pack
  keyNames <- listDirectory dir
  return $ map (parseKey . replace) keyNames
  where
    parseKey :: String -> PubKeyHash
    parseKey key =
      let res = JSON.fromJSON $ JSON.object ["getPubKeyHash" .= key]
       in case res of
            JSON.Success pkh -> pkh
            _ -> error "failed to parse pkh"

getOrFail :: Show e => Either e a -> a
getOrFail = either (error . show) id

getOrFailM :: (Show e, Functor f) => f (Either e b) -> f b
getOrFailM = (getOrFail <$>)
