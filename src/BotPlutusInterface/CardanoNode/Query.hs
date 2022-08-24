-- | Several query functions to query local node
module BotPlutusInterface.CardanoNode.Query (
  NodeQueryError (..),
  NodeConn,
  QueryConstraint,
  queryInCardanoMode,
  queryBabbageEra,
  toQueryError,
  connectionInfo,
) where

import BotPlutusInterface.Types (PABConfig (..))
import Cardano.Api qualified as CApi
import Control.Monad.Freer (Eff, LastMember, Member, send)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except (throwE)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnv)
import Prelude

{- | Error returned in case any error happened querying local node
 (wraps whatever received in `Text`)
-}
data NodeQueryError
  = NodeQueryError Text
  deriving stock (Eq, Show)

type NodeConn = CApi.LocalNodeConnectInfo CApi.CardanoMode

type QueryConstraint effs = (Member (Reader NodeConn) effs, LastMember IO effs)

queryInCardanoMode ::
  forall effs a.
  (QueryConstraint effs) =>
  CApi.QueryInMode CApi.CardanoMode a ->
  Eff effs (Either NodeQueryError a)
queryInCardanoMode query =
  runEitherT $ do
    conn <- lift $ ask @NodeConn
    firstEitherT (NodeQueryError . Text.pack . show) $
      newEitherT $
        send $
          CApi.queryNodeLocalState conn Nothing query

queryBabbageEra ::
  forall effs a.
  (QueryConstraint effs) =>
  CApi.QueryInShelleyBasedEra CApi.BabbageEra a ->
  Eff effs (Either NodeQueryError a)
queryBabbageEra query =
  runEitherT $ do
    result <-
      newEitherT $
        queryInCardanoMode $
          CApi.QueryInEra CApi.BabbageEraInCardanoMode $
            CApi.QueryInShelleyBasedEra CApi.ShelleyBasedEraBabbage query
    case result of
      Right a -> return a
      Left e -> throwE $ toQueryError e

connectionInfo :: PABConfig -> IO (CApi.LocalNodeConnectInfo CApi.CardanoMode)
connectionInfo pabConf =
  CApi.LocalNodeConnectInfo
    (CApi.CardanoModeParams epochSlots)
    (pcNetwork pabConf)
    <$> getEnv "CARDANO_NODE_SOCKET_PATH"
  where
    -- This parameter needed only for the Byron era. Since the Byron
    -- era is over and the parameter has never changed it is ok to
    -- hardcode this. See comment on `Cardano.Api.ConsensusModeParams` in
    -- cardano-node.
    epochSlots = CApi.EpochSlots 21600

toQueryError :: Show e => e -> NodeQueryError
toQueryError = NodeQueryError . Text.pack . show
