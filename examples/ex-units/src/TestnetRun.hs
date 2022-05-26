module TestnetRun (testnetRun) where

import BotPlutusInterface.Contract qualified as BPI
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Aeson (decodeFileStrict, (.=))
import Data.Aeson qualified as JSON
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import GHC.IO.Encoding
import Ledger (PubKeyHash)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (listDirectory)
import System.Environment (getArgs, getEnv, setEnv)
import System.FilePath ((</>))
import TimeDebugContract qualified
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude

testnetRun :: IO ()
testnetRun = do
  setLocaleEncoding utf8
  [bpiDir, cliDir, sockPath, operation] <- getArgs -- /home/mike/dev/mlabs/net-setups/testnet-bpi-setup/data
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
  getEnv "PATH" >>= \p -> setEnv "PATH" (p ++ ":" ++ cliDir)

  cEnv <- mkContractEnv bpiDir

  putStrLn "Running contract"
  res <- case operation of
    "light" -> do
      putStrLn "Running loght debug"
      BPI.runContract cEnv TimeDebugContract.timeDebugLight
    "split" -> do
      putStrLn "Splitting whatever first utxo"
      fmap show <$> BPI.runContract cEnv TimeDebugContract.splitUtxo
    "lock" -> do
      putStrLn "Locking"
      BPI.runContract cEnv TimeDebugContract.lockAtScript
    "unlock" -> do
      putStrLn "Spending"
      BPI.runContract cEnv TimeDebugContract.unlockWithTimeCheck
    other -> error $ "Unsupported operation: " ++ other

  putStrLn $ case res of
    Right r -> "=== OK ===\n" ++ show r
    Left e -> "=== FAILED ===\n" ++ show e

  stats <- readTVarIO (ceContractStats cEnv)
  putStrLn $ "=== Stats ===\n" ++ show stats

mkContractEnv :: Monoid w => FilePath -> IO (ContractEnvironment w)
mkContractEnv bpiDir = do
  let paramsFile = bpiDir </> "pparams.json"
  Just pparams <- decodeFileStrict paramsFile
  contractInstanceID <- ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  contractStats <- newTVarIO (ContractStats mempty)
  pkhs <- getPkhs bpiDir
  return $
    ContractEnvironment
      { cePABConfig = mkPabConf pparams (Text.pack paramsFile) bpiDir (head pkhs),
        ceContractState = contractState,
        ceContractInstanceId = contractInstanceID,
        ceContractStats = contractStats
      }

mkPabConf :: ProtocolParameters -> Text -> FilePath -> PubKeyHash -> PABConfig
mkPabConf pparams pparamsFile bpiDir ownPkh =
  PABConfig
    { pcCliLocation = Local,
      pcNetwork = Testnet (NetworkMagic 1097911063),
      pcChainIndexUrl = BaseUrl Http "localhost" 9083 "",
      pcPort = 9080,
      pcProtocolParams = pparams,
      pcTipPollingInterval = 1_000_000,
      pcOwnPubKeyHash = ownPkh,
      pcOwnStakePubKeyHash = Nothing,
      pcScriptFileDir = Text.pack $ bpiDir </> "scripts",
      pcSigningKeyFileDir = Text.pack $ bpiDir </> "signing-keys",
      pcTxFileDir = Text.pack $ bpiDir </> "txs",
      pcDryRun = False,
      pcLogLevel = Error,
      pcProtocolParamsFile = pparamsFile,
      pcEnableTxEndpoint = True,
      pcCollectStats = True,
      pcMetadataDir = Text.pack $ bpiDir </> "metadata"
    }

getPkhs :: FilePath -> IO [PubKeyHash]
getPkhs bpiDir = do
  let dir = bpiDir </> "signing-keys"
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

-- getOrFail :: Show e => Either e a -> a
-- getOrFail = either (error . show) id

-- getOrFailM :: (Show e, Functor f) => f (Either e b) -> f b
-- getOrFailM = (getOrFail <$>)
