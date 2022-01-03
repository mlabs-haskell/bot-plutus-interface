{-# LANGUAGE AllowAmbiguousTypes #-}

module MLabsPAB.Server (app, initState) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, retry)
import Control.Monad (forever, guard, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Either.Combinators (leftToMaybe)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Row (Row)
import Data.UUID.V4 qualified as UUID
import MLabsPAB.Contract (runContract)
import MLabsPAB.Types (
  AppState (AppState),
  ContractEnvironment (..),
  ContractState (ContractState),
  PABConfig,
  SomeContractState (SomeContractState),
 )
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
import Servant.API (JSON, Post, ReqBody, (:<|>) (..), (:>))
import Servant.API.WebSocket (WebSocketPending)
import Servant.Server (Application, Handler, Server, serve)
import Wallet.Emulator (Wallet, knownWallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

initState :: IO AppState
initState = AppState <$> newTVarIO Map.empty

-- | Mock API Schema, stripped endpoints that we don't use in this project
type API a =
  ("ws" :> WebSocketPending) -- Combined websocket (subscription protocol)
    :<|> ( "api"
            :> "contract"
            :> "activate"
            :> ReqBody '[JSON] (ContractActivationArgs a)
            :> Post '[JSON] ContractInstanceId -- Start a new instance.
         )

server :: HasDefinitions t => PABConfig -> AppState -> Server (API t)
server pabConfig state =
  websocketHandler state :<|> activateContractHandler pabConfig state

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
    observeUpdates contractState Nothing
  where
    observeUpdates :: forall (w :: Type). ToJSON w => TVar (ContractState w) -> Maybe (ContractState w) -> IO ()
    observeUpdates contractState prevHandled = do
      statusUpdate <- atomically $ do
        result <- readTVar contractState

        let msgs = handleContractActivityChange contractInstanceID prevHandled result
        guard (not (null msgs))
        pure result

      mapM_ (sendTextData conn . JSON.encode) $
        handleContractActivityChange contractInstanceID prevHandled statusUpdate

      unless (isFinished statusUpdate) $ observeUpdates contractState (Just statusUpdate)

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
handleContractActivityChange
  contractInstanceID
  (Just (ContractState prevA prevW))
  (ContractState a w) =
    catMaybes [activityChange, observableStateChange]
    where
      activityChange =
        if prevA /= a
          then case a of
            Done maybeError -> do
              Just $ InstanceUpdate contractInstanceID $ ContractFinished maybeError
            _ -> Nothing
          else Nothing

      observableStateChange =
        if toJSON prevW /= toJSON w
          then Just $ InstanceUpdate contractInstanceID $ NewObservableState (toJSON w)
          else Nothing
handleContractActivityChange _ _ _ = []

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
activateContractHandler pabConf state (ContractActivationArgs cardMessage maybeWallet) =
  let wallet = fromMaybe (knownWallet 1) maybeWallet
   in case getContract cardMessage of
        SomeBuiltin contract -> handleContract pabConf wallet state contract

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
  Wallet ->
  AppState ->
  contract w s e a ->
  Handler ContractInstanceId
handleContract pabConf wallet state@(AppState st) contract = liftIO $ do
  contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)

  atomically $ modifyTVar st (Map.insert contractInstanceID (SomeContractState contractState))

  let contractEnv =
        ContractEnvironment
          { cePABConfig = pabConf
          , ceContractState = contractState
          , ceWallet = wallet
          , ceContractInstanceId = contractInstanceID
          , ceOwnPubKey = "5842d469074913a4a0e8e3ec3b4d46eded2076c186735135d1f5e6ef592984d7"
          }
  void $
    forkIO $ do
      result <- runContract contractEnv wallet (toContract contract)
      let maybeError = toJSON <$> leftToMaybe result
      broadcastContractResult @w state contractInstanceID maybeError
  pure contractInstanceID
