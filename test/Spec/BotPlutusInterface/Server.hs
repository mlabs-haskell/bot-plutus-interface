module Spec.BotPlutusInterface.Server (tests) where

import BotPlutusInterface.Files (txFileName)
import BotPlutusInterface.Server (RawTxEndpoint, TxIdCapture (TxIdCapture), app, initState)
import BotPlutusInterface.Types (
  HasDefinitions (..),
  PABConfig (..),
  RawTx (..),
  SomeBuiltin (..),
 )

import Ledger.TxId (TxId)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (===))

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.API (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )
import Servant.Client (ClientEnv, ClientError (..), client, mkClientEnv, responseStatusCode, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), parseBaseUrl)

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Proxy (Proxy (..))
import Data.Text (pack, unpack)
import Data.Void (Void, absurd)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Prelude

type RawTxEndpointResponse = Either ClientError RawTx
type RawTxTest a = (TxId -> IO RawTxEndpointResponse) -> IO a

tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.Server"
    [ rawTxTests
    ]

rawTxTests :: TestTree
rawTxTests =
  testGroup
    "rawTx"
    [ testCase "Can fetch valid tx file" fetchTx
    , testCase "Returns 404 for missing txs" fetchMissingTx
    , testCase "Returns 404 for valid request when the endpoint is disabled" fetchWithDefaultConfig
    , testProperty "TxId URL encoding reversible" txIdReversible
    ]
  where
    fetchTx :: IO ()
    fetchTx = do
      initServerAndClient enableTxEndpointConfig $ \runRawTxClient -> do
        result <- runRawTxClient txHash
        result @?= Right rawTx

    fetchMissingTx :: IO ()
    fetchMissingTx = do
      initServerAndClient enableTxEndpointConfig $ \runRawTxClient -> do
        Left (FailureResponse _ res) <- runRawTxClient txHash2
        responseStatusCode res @?= status404

    fetchWithDefaultConfig :: IO ()
    fetchWithDefaultConfig = do
      initServerAndClient def $ \runRawTxClient -> do
        Left (FailureResponse _ res) <- runRawTxClient txHash
        responseStatusCode res @?= status404

    txIdReversible :: TxIdCapture -> Property
    txIdReversible txId = parseUrlPiece (toUrlPiece txId) === Right txId

txProxy :: Proxy RawTxEndpoint
txProxy = Proxy

initServerAndClient :: PABConfig -> RawTxTest a -> IO a
initServerAndClient config test = do
  withSystemTempDirectory "tx" $ \path -> do
    let pabConfig :: PABConfig
        pabConfig = config {pcTxFileDir = pack path}
    state <- initState
    LBS.writeFile (path </> testTxFileName) txFileContents
    testWithApplication (pure $ app @EmptyContract pabConfig state) (initClientOnPort test)
  where
    initClientOnPort :: RawTxTest a -> Int -> IO a
    initClientOnPort testToRun port = do
      baseUrl <- parseBaseUrl "http://localhost"
      manager <- newManager defaultManagerSettings

      let clientEnv :: ClientEnv
          clientEnv = mkClientEnv manager $ baseUrl {baseUrlPort = port}

          runRawTxClient :: TxId -> IO RawTxEndpointResponse
          runRawTxClient txId = runClientM (client txProxy (TxIdCapture txId)) clientEnv

      testToRun runRawTxClient

txHash :: TxId
txHash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

txHash2 :: TxId
txHash2 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

testTxFileName :: FilePath
testTxFileName = unpack $ txFileName txHash "raw"

rawTx :: RawTx
rawTx =
  RawTx
    { _type = "TxBodyAlonzo"
    , _description = "description"
    , _cborHex = "hex"
    }

txFileContents :: LBS.ByteString
txFileContents = encode rawTx

enableTxEndpointConfig :: PABConfig
enableTxEndpointConfig = def {pcEnableTxEndpoint = True}

-- Since we are not testing the contract endpoints we just use a newtype around Void as a Contract
newtype EmptyContract = EmptyContract {unEmptyContract :: Void}
  deriving newtype (FromJSON, ToJSON)

instance HasDefinitions EmptyContract where
  getDefinitions :: [EmptyContract]
  getDefinitions = []

  getSchema :: EmptyContract -> [FunctionSchema FormSchema]
  getSchema = absurd . unEmptyContract

  getContract :: (EmptyContract -> SomeBuiltin)
  getContract = absurd . unEmptyContract
