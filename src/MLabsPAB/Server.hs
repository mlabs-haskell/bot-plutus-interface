{-# LANGUAGE AllowAmbiguousTypes #-}

module MLabsPAB.Server (app, State, initState) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, retry)
import Control.Monad (forever, guard, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Either.Combinators (leftToMaybe)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.UUID.V4 qualified as UUID
import MLabsPAB.Contract (runContract)
import MLabsPAB.Types (ContractEnvironment (..), PABConfig)
import Network.WebSockets (
  Connection,
  PendingConnection,
  acceptRequest,
  receiveData,
  sendTextData,
  withPingThread,
 )
import Plutus.Contract.Types (IsContract (toContract))
import Plutus.PAB.Effects.Contract.Builtin (ContractConstraints, HasDefinitions, SomeBuiltin (..), getContract)
import Plutus.PAB.Webserver.Types (
  CombinedWSStreamToClient (InstanceUpdate),
  CombinedWSStreamToServer (Subscribe),
  ContractActivationArgs (..),
  InstanceStatusToClient (ContractFinished),
 )
import Servant.API (JSON, Post, ReqBody, (:<|>) (..), (:>))
import Servant.API.WebSocket (WebSocketPending)
import Servant.Server (Application, Handler, Server, serve)
import Wallet.Emulator (Wallet, knownWallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

newtype State = State (TVar (Map ContractInstanceId InstanceStatusToClient))

initState :: IO State
initState = State <$> newTVarIO Map.empty

-- | Mock API Schema, stripped endpoints that we don't use in this project
type API a =
  ("ws" :> WebSocketPending) -- Combined websocket (subscription protocol)
    :<|> ( "api"
            :> "contract"
            :> "activate"
            :> ReqBody '[JSON] (ContractActivationArgs a)
            :> Post '[JSON] ContractInstanceId -- Start a new instance.
         )

server :: HasDefinitions t => PABConfig -> State -> Server (API t)
server pabConfig state =
  websocketHandler state :<|> activateContractHandler pabConfig state

apiProxy :: forall (t :: Type). Proxy (API t)
apiProxy = Proxy

app :: forall (t :: Type). (HasDefinitions t, FromJSON t) => PABConfig -> State -> Application
app pabConfig state = serve (apiProxy @t) $ server pabConfig state

-- | Mock websocket handler (can only send ContractFinished message)
websocketHandler :: State -> PendingConnection -> Handler ()
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
subscribeToContract :: Connection -> State -> ContractInstanceId -> IO ThreadId
subscribeToContract conn (State s) contractInstanceID =
  forkIO $ do
    putStrLn $ "WebSocket subscribed to " ++ show contractInstanceID
    observeUpdates Nothing
  where
    observeUpdates prevHandled = do
      statusUpdate <- atomically $ do
        finishedInstances <- readTVar s

        case Map.lookup contractInstanceID finishedInstances of
          Just result -> do
            guard (Just result /= prevHandled)
            pure result
          Nothing -> retry

      let msg = InstanceUpdate contractInstanceID statusUpdate
      sendTextData conn $ JSON.encode msg

      unless (isFinished statusUpdate) $ observeUpdates (Just statusUpdate)

    isFinished (ContractFinished _) = True
    isFinished _ = False

-- | Broadcast a contract update to subscribers
broadcastContractResult :: State -> ContractInstanceId -> InstanceStatusToClient -> IO ()
broadcastContractResult (State s) contractInstanceID statusUpdateMsg =
  atomically $
    modifyTVar s $ Map.insert contractInstanceID statusUpdateMsg

-- | This handler will call the corresponding contract endpoint handler
activateContractHandler ::
  HasDefinitions c =>
  PABConfig ->
  State ->
  ContractActivationArgs c ->
  Handler ContractInstanceId
activateContractHandler pabConf state (ContractActivationArgs cardMessage maybeWallet) =
  let wallet = fromMaybe (knownWallet 1) maybeWallet
   in case getContract cardMessage of
        SomeBuiltin contract -> handleContract pabConf wallet state contract

handleContract ::
  forall w s e a contract.
  ( ContractConstraints w s e
  , IsContract contract
  ) =>
  PABConfig ->
  Wallet ->
  State ->
  contract w s e a ->
  Handler ContractInstanceId
handleContract pabConf wallet state contract = liftIO $ do
  contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
  let contractEnv =
        ContractEnvironment
          { cePABConfig = pabConf
          , ceWallet = wallet
          , ceContractInstanceId = contractInstanceID
          }
  void $
    forkIO $ do
      result <- runContract contractEnv wallet (toContract contract)
      let maybeErrors = leftToMaybe $ fst result
      let updateMsg = ContractFinished (toJSON <$> maybeErrors)
      liftIO . logErrors $ toJSON <$> maybeErrors
      broadcastContractResult state contractInstanceID updateMsg
  pure contractInstanceID
  where
    logErrors =   
      maybe 
        (putStrLn "Contract executed") 
        (putStrLn . ("Execution ERROR: " <>) . show)
