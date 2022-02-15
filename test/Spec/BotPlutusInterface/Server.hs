module Spec.BotPlutusInterface.Server (tests) where

import BotPlutusInterface.Server (app, initState)
import BotPlutusInterface.Types (
  HasDefinitions (..),
  PABConfig (..),
  RawTx (..),
  SomeBuiltin (..),
 )

import Playground.Types (FunctionSchema)
import Schema (FormSchema)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.API (Capture, Get, JSON, (:>))
import Servant.Client (ClientEnv, ClientError (..), client, mkClientEnv, responseStatusCode, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), parseBaseUrl)

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack, unpack)
import Data.Void (Void, absurd)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Prelude

type RawTxEndpointResponse = Either ClientError RawTx
type RawTxTest a = (Text -> IO RawTxEndpointResponse) -> IO a

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
    , testCase "If an extension is supplied, it is replaced by .raw" fetchSignedTx
    , testCase "Unable to fetch outside tx folder" fetchOutsideTxFolder
    , testCase "Returns 404 for valid request when the endpoint is disabled" fetchWithDefaultConfig
    ]
  where
    fetchTx :: IO ()
    fetchTx = do
      initServerAndClient enableTxEndpointConfig $ \runRawTxClient -> do
        result <- runRawTxClient txHash
        result @?= Right rawTx

    fetchSignedTx :: IO ()
    fetchSignedTx = do
      initServerAndClient enableTxEndpointConfig $ \runRawTxClient -> do
        result <- runRawTxClient $ txHash <> ".signed"
        result @?= Right rawTx

    fetchOutsideTxFolder :: IO ()
    fetchOutsideTxFolder = do
      initServerAndClient enableTxEndpointConfig $ \runRawTxClient -> do
        Left (FailureResponse _ res) <- runRawTxClient "../somefile"
        responseStatusCode res @?= status404

    fetchWithDefaultConfig :: IO ()
    fetchWithDefaultConfig = do
      initServerAndClient def $ \runRawTxClient -> do
        Left (FailureResponse _ res) <- runRawTxClient txHash
        responseStatusCode res @?= status404

-- Ideally we would reuse the API type definition from BotPlutusInterface.Server but servant-client
-- can not generate a client for the websocket endpoint.
txProxy ::
  Proxy
    ( "rawTx"
        :> Capture "hash" Text
        :> Get '[JSON] RawTx
    )
txProxy = Proxy

initServerAndClient :: PABConfig -> RawTxTest a -> IO a
initServerAndClient config test = do
  withSystemTempDirectory "tx" $ \path -> do
    let pabConfig :: PABConfig
        pabConfig = config {pcTxFileDir = pack path}
    state <- initState
    LBS.writeFile (path </> txFileName) txFileContents
    testWithApplication (pure $ app @EmptyContract pabConfig state) (initClientOnPort test)
  where
    initClientOnPort :: RawTxTest a -> Int -> IO a
    initClientOnPort testToRun port = do
      baseUrl <- parseBaseUrl "http://localhost"
      manager <- newManager defaultManagerSettings

      let clientEnv :: ClientEnv
          clientEnv = mkClientEnv manager $ baseUrl {baseUrlPort = port}

          runRawTxClient :: Text -> IO RawTxEndpointResponse
          runRawTxClient hash = runClientM (client txProxy hash) clientEnv

      testToRun runRawTxClient

txHash :: Text
txHash = "test"

txFileName :: FilePath
txFileName = "tx-" <> unpack txHash <> ".raw"

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
