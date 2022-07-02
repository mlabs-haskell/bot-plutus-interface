{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.Server (
  app,
  initState,
  WebSocketEndpoint,
  ActivateContractEndpoint,
  RawTxEndpoint,
  TxIdCapture (TxIdCapture),
) where

import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Files (txFileName, txIdToText)
import BotPlutusInterface.Types (
  AppState (AppState),
  CollateralVar (CollateralVar),
  ContractEnvironment (..),
  ContractState (ContractState, csActivity, csObservableState),
  PABConfig (..),
  RawTx,
  SomeContractState (SomeContractState),
 )
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, retry)
import Control.Monad (forever, guard, unless, void)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Combinators (leftToMaybe)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (Proxy))
import Data.Row (Row)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 qualified as UUID
import Ledger.TxId (TxId (TxId))
import Network.WebSockets (
  Connection,
  PendingConnection,
  acceptRequest,
  receiveData,
  sendTextData,
  withPingThread,
 )
import Plutus.Contract.Types (IsContract (toContract))
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active, Done))
import Plutus.PAB.Effects.Contract.Builtin (
  ContractConstraints,
  HasDefinitions,
  SomeBuiltin (..),
  getContract,
 )
import Plutus.PAB.Webserver.Types (
  CombinedWSStreamToClient (InstanceUpdate),
  CombinedWSStreamToServer (Subscribe),
  ContractActivationArgs (..),
  InstanceStatusToClient (ContractFinished, NewObservableState),
 )
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes), fromHex)
import PlutusTx.Prelude (lengthOfByteString)
import Servant.API (
  Capture,
  FromHttpApiData (parseUrlPiece),
  Get,
  JSON,
  Post,
  ReqBody,
  ToHttpApiData (toUrlPiece),
  (:<|>) ((:<|>)),
  (:>),
 )
import Servant.API.WebSocket (WebSocketPending)
import Servant.Server (Application, Handler, Server, err404, serve)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))
import Test.QuickCheck (Arbitrary (arbitrary), elements, vectorOf)
import Wallet.Types (ContractInstanceId (..))
import Prelude

initState :: IO AppState
initState = AppState <$> newTVarIO Map.empty

-- | Mock API Schema, stripped endpoints that we don't use in this project
type API a = WebSocketEndpoint :<|> ActivateContractEndpoint a :<|> ContractLookupEndpoint :<|> RawTxEndpoint

-- Endpoints are split up so it is easier to test them. In particular servant-client
-- can not generate a client for the WebSocketEndpoint; this allows us to still
-- use servant-client to test the other endpoints

type WebSocketEndpoint = "ws" :> WebSocketPending -- Combined websocket (subscription protocol)

type ActivateContractEndpoint a =
  "api"
    :> "contract"
    :> "activate"
    :> ReqBody '[JSON] (ContractActivationArgs a)
    :> Post '[JSON] ContractInstanceId -- Start a new instance.

type ContractLookupEndpoint =
  "api"
    :> "contract"
    :> "exists"
    :> ReqBody '[JSON] ContractInstanceId
    :> Post '[JSON] Bool

type RawTxEndpoint =
  "raw-tx"
    :> Capture "tx-id" TxIdCapture
    :> Get '[JSON] RawTx

newtype TxIdCapture = TxIdCapture TxId
  deriving newtype (Eq, Show)

instance FromHttpApiData TxIdCapture where
  parseUrlPiece :: Text -> Either Text TxIdCapture
  parseUrlPiece t = bimap pack bytesToTxIdCapture $ checkLength =<< fromHex (encodeUtf8 t)
    where
      checkLength :: LedgerBytes -> Either String LedgerBytes
      checkLength b@(LedgerBytes bs) =
        if lengthOfByteString bs == 32
          then Right b
          else Left "Invalid length"
      bytesToTxIdCapture :: LedgerBytes -> TxIdCapture
      bytesToTxIdCapture (LedgerBytes b) = TxIdCapture $ TxId b

instance ToHttpApiData TxIdCapture where
  toUrlPiece (TxIdCapture txId) = txIdToText txId

instance Arbitrary TxIdCapture where
  arbitrary = TxIdCapture . fromString <$> vectorOf 64 (elements "0123456789abcdefABCDEF")

server :: HasDefinitions t => PABConfig -> AppState -> Server (API t)
server pabConfig state =
  websocketHandler state
    :<|> activateContractHandler pabConfig state
    :<|> contractLookupHandler state
    :<|> rawTxHandler pabConfig

apiProxy :: forall (t :: Type). Proxy (API t)
apiProxy = Proxy

app :: forall (t :: Type). (HasDefinitions t, FromJSON t) => PABConfig -> AppState -> Application
app pabConfig state = serve (apiProxy @t) $ server pabConfig state

-- | Mock websocket handler (can only send ContractFinished message)
websocketHandler :: AppState -> PendingConnection -> Handler ()
websocketHandler state pendingConn = liftIO $ do
  conn <- acceptRequest pendingConn

  withPingThread conn 30 (return ()) $
    forever $ do
      msg <- receiveData conn

      case JSON.eitherDecode msg of
        Right (Subscribe (Left contractInstID)) ->
          void $ subscribeToContract conn state contractInstID
        _ -> pure ()

{- | Create a thread subscribing to state changes on a specific contract instance
 and send a websocket response on each change
-}
subscribeToContract :: Connection -> AppState -> ContractInstanceId -> IO ThreadId
subscribeToContract conn (AppState s) contractInstanceID =
  forkIO $ do
    putStrLn $ "WebSocket subscribed to " ++ show contractInstanceID
    SomeContractState contractState <- atomically $ do
      instances <- readTVar s
      maybe retry pure $ Map.lookup contractInstanceID instances
    putStrLn "Found instance"
    observeUpdates contractState Nothing
  where
    observeUpdates :: forall (w :: Type). ToJSON w => TVar (ContractState w) -> Maybe (ContractState w) -> IO ()
    observeUpdates contractState prevHandled = do
      (lastStatus, msgs) <- atomically $ do
        result <- readTVar contractState

        let msgs = handleContractActivityChange contractInstanceID prevHandled result
        guard (not (null msgs))
        pure (result, msgs)

      mapM_ (sendTextData conn . JSON.encode) msgs

      unless (isFinished lastStatus) $ observeUpdates contractState (Just lastStatus)

    isFinished (ContractState (Done _) _) = True
    isFinished _ = False

-- | Detects changes between states and composes the messages to be sent via the websocket channel
handleContractActivityChange ::
  forall (w :: Type).
  ToJSON w =>
  ContractInstanceId ->
  Maybe (ContractState w) ->
  ContractState w ->
  [CombinedWSStreamToClient]
handleContractActivityChange contractInstanceID prevState currentState =
  catMaybes [observableStateChange, activityChange]
  where
    activityChange =
      if (csActivity <$> prevState) /= Just currentState.csActivity
        then case currentState.csActivity of
          Done maybeError -> do
            Just $ InstanceUpdate contractInstanceID $ ContractFinished maybeError
          _ -> Nothing
        else Nothing

    observableStateChange =
      if (toJSON . csObservableState <$> prevState) /= Just (toJSON currentState.csObservableState)
        then
          Just $
            InstanceUpdate contractInstanceID $
              NewObservableState (toJSON currentState.csObservableState)
        else Nothing

-- | Broadcast a contract update to subscribers
broadcastContractResult ::
  forall (w :: Type).
  (Monoid w, ToJSON w) =>
  AppState ->
  ContractInstanceId ->
  Maybe JSON.Value ->
  IO ()
broadcastContractResult (AppState st) contractInstanceID maybeError = do
  state <- readTVarIO st
  case Map.lookup contractInstanceID state of
    Nothing -> do
      contractState <- newTVarIO $ ContractState (Done maybeError) (mempty :: w)
      atomically $ modifyTVar st (Map.insert contractInstanceID (SomeContractState contractState))
    Just (SomeContractState cs) -> atomically $
      modifyTVar cs $ \(ContractState _ w) ->
        ContractState (Done maybeError) w

-- | This handler will call the corresponding contract endpoint handler
activateContractHandler ::
  forall (c :: Type).
  HasDefinitions c =>
  PABConfig ->
  AppState ->
  ContractActivationArgs c ->
  Handler ContractInstanceId
activateContractHandler pabConf state (ContractActivationArgs cardMessage _) =
  case getContract cardMessage of
    SomeBuiltin contract -> handleContract pabConf state contract

contractLookupHandler ::
  AppState ->
  ContractInstanceId ->
  Handler Bool
contractLookupHandler (AppState s) contractInstanceId = liftIO . atomically $ do
  instances <- readTVar s
  return $ Map.member contractInstanceId instances

handleContract ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( ContractConstraints w s e
  , IsContract contract
  ) =>
  PABConfig ->
  AppState ->
  contract w s e a ->
  Handler ContractInstanceId
handleContract pabConf state@(AppState st) contract = liftIO $ do
  contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  contractStats <- newTVarIO mempty
  contractLogs <- newTVarIO mempty
  collateral <- CollateralVar <$> newTVarIO Nothing

  atomically $ modifyTVar st (Map.insert contractInstanceID (SomeContractState contractState))

  let contractEnv =
        ContractEnvironment
          { cePABConfig = pabConf
          , ceContractState = contractState
          , ceContractInstanceId = contractInstanceID
          , ceContractStats = contractStats
          , ceContractLogs = contractLogs
          , ceCollateral = collateral
          }
  void $
    forkIO $ do
      result <- runContract contractEnv (toContract contract)
      let maybeError = toJSON <$> leftToMaybe result
      broadcastContractResult @w state contractInstanceID maybeError
  pure contractInstanceID

-- | This handler will allow to retrieve raw transactions from the pcTxFileDir if pcEnableTxEndpoint is True
rawTxHandler :: PABConfig -> TxIdCapture -> Handler RawTx
rawTxHandler config (TxIdCapture txId) = do
  -- Check that endpoint is enabled
  assert config.pcEnableTxEndpoint
  -- Absolute path to pcTxFileDir that is specified in the config
  txFolderPath <- liftIO $ makeAbsolute (unpack config.pcTxFileDir)

  -- Create full path
  let path :: FilePath
      path = txFolderPath </> unpack (txFileName txId "raw")

  -- ensure file exists
  fileExists <- liftIO $ doesFileExist path
  assert fileExists

  contents <- liftIO $ LBS.readFile path
  case JSON.decode contents of
    Just rawTx -> pure rawTx
    Nothing -> throwError err404
  where
    assert :: Bool -> Handler ()
    assert True = pure ()
    assert False = throwError err404
