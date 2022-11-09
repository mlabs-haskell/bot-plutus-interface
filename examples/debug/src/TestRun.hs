module TestRun (testnetRun) where

import BotPlutusInterface.Contract qualified as BPI
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Mainnet, Testnet), NetworkMagic (NetworkMagic))
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
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
import System.Random (Random (randomR), newStdGen, randomRIO)
import Tools
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude
import LockSpendMint qualified
import BotPlutusInterface.Types (LogType(CollateralLog))

testnetRun :: IO ()
testnetRun = do
  setLocaleEncoding utf8
  [bpiDir, cliDir, sockPath, netMagic, operation] <- getArgs -- /home/mike/dev/mlabs/net-setups/testnet-bpi-setup/data
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
  getEnv "PATH" >>= \p -> setEnv "PATH" (p ++ ":" ++ cliDir)

  let netMagic' = read netMagic

  cEnv <- mkContractEnv netMagic' bpiDir

  putStrLn "Running contract"

  stats <- readTVarIO (ceContractStats cEnv)
  putStrLn $ "=== Stats ===\n" ++ show stats
  void $ runMyContract cEnv operation
  where
    runMyContract cEnv operation = do
      res <- case operation of
        _ -> BPI.runContract cEnv LockSpendMint.lockThenSpend

      case res of
        -- Right r -> "=== OK ===\n" ++ show r >> runMyContract
        Right r -> do
          putStrLn ("=== OK ===\n" ++ show r)
          -- randomDelay
          -- runMyContract cEnv operation
        Left e -> error ("=== FAILED ===\n" ++ show e)

    randomDelay :: IO ()
    randomDelay = do
      g <- newStdGen
      let (t, _) = randomR (0, 2_000_000) g
      putStrLn $ "delay: " ++ show t
      threadDelay t

type NetMagic = Integer -- 0 fot mainnet, 1097911063 public testnet

mkContractEnv :: Monoid w => NetMagic -> FilePath -> IO (ContractEnvironment w)
mkContractEnv netMagic bpiDir = do
  let paramsFile = bpiDir </> "pparams.json"
  Just pparams <- decodeFileStrict paramsFile
  contractInstanceID <- ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  contractStats <- newTVarIO (ContractStats mempty)
  contractLogs <- newTVarIO (LogsList mempty)
  collateral <- CollateralVar <$> newTVarIO Nothing
  pkhs <- getPkhs bpiDir
  return $
    ContractEnvironment
      { cePABConfig = mkPabConf netMagic pparams (Text.pack paramsFile) bpiDir (head pkhs)
      , ceContractState = contractState
      , ceContractInstanceId = contractInstanceID
      , ceContractStats = contractStats
      , ceContractLogs = contractLogs
      , ceCollateral = collateral
      }

mkPabConf :: NetMagic -> ProtocolParameters -> Text -> FilePath -> PubKeyHash -> PABConfig
mkPabConf netMagic pparams pparamsFile bpiDir ownPkh =
  PABConfig
    { pcCliLocation = Local
    , pcNetwork = netId
    , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
    , pcPort = 9080
    , pcProtocolParams = Just pparams
    , pcTipPollingInterval = 1_000_000
    , pcOwnPubKeyHash = ownPkh
    , pcOwnStakePubKeyHash = Nothing
    , pcScriptFileDir = Text.pack $ bpiDir </> "scripts"
    , pcSigningKeyFileDir = Text.pack $ bpiDir </> "signing-keys"
    , pcTxFileDir = Text.pack $ bpiDir </> "txs"
    , pcDryRun = False
    , pcLogLevel = Error [AnyLog]
    , pcProtocolParamsFile = pparamsFile
    , pcEnableTxEndpoint = False
    , pcCollectStats = False
    , pcCollectLogs = False
    , pcBudgetMultiplier = 1
    , pcMetadataDir = Text.pack $ bpiDir </> "metadata"
    , pcTxStatusPolling = TxStatusPolling 500_000 5
    , pcCollateralSize = 10_000_000
    }
  where
    netId = case netMagic of
      0 -> Mainnet
      other -> Testnet . NetworkMagic . fromInteger $ other

getPkhs :: FilePath -> IO [PubKeyHash]
getPkhs bpiDir = do
  let dir = bpiDir </> "signing-keys"
      replace =
        Text.unpack
          . Text.replace "signing-key-" ""
          . Text.replace ".skey" ""
          . Text.pack
  keyNames <- listDirectory dir
  return $ map (pkhFromHash . replace) keyNames

-- getOrFail :: Show e => Either e a -> a
-- getOrFail = either (error . show) id

-- getOrFailM :: (Show e, Functor f) => f (Either e b) -> f b
-- getOrFailM = (getOrFail <$>)
