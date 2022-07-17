module TestRun (testnetRun, main) where

import BotPlutusInterface.Contract qualified as BPI
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Mainnet))
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
import Data.Aeson (decodeFileStrict)
import Data.Text (Text)
import Data.String (fromString)

import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import GHC.IO.Encoding
import Ledger (PubKeyHash)
import Ledger.Value qualified as Value
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import SomeDebugContract qualified
import System.Directory (listDirectory)
import System.Environment (getArgs, getEnv, setEnv)
import System.FilePath ((</>))
import TimeDebugContract qualified
import Tools
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude

main = testnetRun

testnetRun :: IO ()
testnetRun = do
  setLocaleEncoding utf8
  [operation] <- getArgs

  bpiDir <- getEnv "BPI_PATH"

  print bpiDir

  sockPath <- getEnv "CARDANO_NODE_SOCKET_PATH"

  -- cliDir <- getEnv "/usr/"
  -- let bpiDir =
  let cliDir = "/nix/st4ore/s76zj58fp6fvgmv2id76xib4sni81yvz-cardano-cli-exe-cardano-cli-1.34.1/bin"
      netMagic = "0"
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
  getEnv "PATH" >>= \p -> setEnv "PATH" (p ++ ":" ++ cliDir)

  let netMagic' = read netMagic


  cEnv <- mkContractEnv netMagic' bpiDir

  putStrLn "Running contract"

  void $ runMyContract cEnv operation
  collateral <- readTVarIO . unCollateralVar . ceCollateral $ cEnv
  putStrLn $ "Collateral env: " <> show collateral
  where
    runMyContract cEnv operation = do
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
        "lock-unlock" -> do
          putStrLn "Lock -> Unlock"
          BPI.runContract cEnv TimeDebugContract.lockUnlock
        "viaPay" -> do
          putStrLn "Debug range validation and mempool. Specify interval:"
          int <- read <$> getLine
          BPI.runContract cEnv (TimeDebugContract.timeDebugViaPay int)
        "collateral" -> do
          putStrLn "payToHardcodedPKH"
          BPI.runContract cEnv SomeDebugContract.payToHardcodedPKH >> pure (Right "Done")
        "mint" -> do
          putStrLn "minting, provide the token name to be minted: "
          BPI.runContract cEnv (SomeDebugContract.mintContract "diamond") >> pure (Right "Done")
        other -> error $ "Unsupported operation: " ++ other

      case res of
        Right r -> do
          putStrLn ("=== OK ===\n" ++ show r)
        Left e -> putStrLn ("=== FAILED ===\n" ++ show e)

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
  pure $
    ContractEnvironment
      { cePABConfig = mkPabConf netMagic pparams (Text.pack paramsFile) bpiDir (head pkhs)
      , ceContractState = contractState
      , ceContractInstanceId = contractInstanceID
      , ceContractStats = contractStats
      , ceContractLogs = contractLogs
      , ceCollateral = collateral
      }

mkPabConf :: NetMagic -> ProtocolParameters -> Text -> FilePath -> PubKeyHash -> PABConfig
mkPabConf _ pparams pparamsFile bpiDir ownPkh =
  PABConfig
    { pcCliLocation = Local
    , pcNetwork = Mainnet
    , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
    , pcPort = 9080
    , pcProtocolParams = pparams
    , pcTipPollingInterval = 1_000_000
    , pcOwnPubKeyHash = ownPkh
    , pcOwnStakePubKeyHash = Nothing
    , pcScriptFileDir = Text.pack $ bpiDir </> "scripts"
    , pcSigningKeyFileDir = Text.pack $ bpiDir </> "signing-keys"
    , pcTxFileDir = Text.pack $ bpiDir </> "txs"
    , pcDryRun = False
    , pcLogLevel = Debug
    , pcProtocolParamsFile = pparamsFile
    , pcEnableTxEndpoint = False
    , pcCollectStats = False
    , pcCollectLogs = False
    , pcBudgetMultiplier = 1
    , pcMetadataDir = Text.pack $ bpiDir </> "metadata"
    , pcTxStatusPolling = TxStatusPolling 500_000 5
    , pcCollateralSize = 10_248_256
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
  pure $ map (pkhFromHash . replace) keyNames
