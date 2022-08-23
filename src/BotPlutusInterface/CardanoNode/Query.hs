-- | Several query functions to query local node
module BotPlutusInterface.CardanoNode.Query (
  NodeQueryError (..),
  NodeConn,
  QueryConstraint,
  queryInCardanoMode,
  queryBabbageEra,
  toQueryError,
) where

import Cardano.Api qualified as CApi
import Control.Monad.Freer (Eff, LastMember, Member, send)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except (throwE)
import Data.Text (Text)
import Data.Text qualified as Text
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
      Left e -> throwE $ NodeQueryError $ Text.pack $ show e

-- data NodeInfo = NodeInfo
--   { niNetworkId :: C.NetworkId
--   , niSocket :: FilePath
--   }
--
-- queryProtocolParams :: NodeInfo -> IO (Either NodeQueryError ProtocolParameters)
-- queryProtocolParams (connectionInfo -> cInfo) =
--   flattenQueryResult <$> C.queryNodeLocalState cInfo Nothing query
--   where
--     query =
--       C.QueryInEra C.BabbageEraInCardanoMode $
--         C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters
--
-- querySystemStart :: NodeInfo -> IO (Either NodeQueryError SystemStart)
-- querySystemStart (connectionInfo -> cInfo) =
--   left toQueryError
--     <$> C.queryNodeLocalState
--       cInfo
--       Nothing
--       C.QuerySystemStart
--
-- queryEraHistory :: NodeInfo -> IO (Either NodeQueryError (C.EraHistory C.CardanoMode))
-- queryEraHistory (connectionInfo -> cInfo) =
--   left toQueryError
--     <$> C.queryNodeLocalState
--       cInfo
--       Nothing
--       (C.QueryEraHistory C.CardanoModeIsMultiEra)
--
-- queryOutsByInputs :: NodeInfo -> [C.TxIn] -> IO (Either NodeQueryError (C.UTxO C.BabbageEra))
-- queryOutsByInputs (connectionInfo -> cInfo) ins =
--   flattenQueryResult
--     <$> C.queryNodeLocalState
--       cInfo
--       Nothing
--       query
--   where
--     query =
--       C.QueryInEra C.BabbageEraInCardanoMode $
--         C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage $
--           C.QueryUTxO (C.QueryUTxOByTxIn (Set.fromList ins))
--
-- flattenQueryResult ::
--   (Show e1, Show e2, Show b) =>
--   Either e1 (Either e2 b) ->
--   Either NodeQueryError b
-- flattenQueryResult = \case
--   Right (Right res) -> Right res
--   err -> Left $ NodeQueryError (pack $ show err)
--
-- connectionInfo :: NodeInfo -> C.LocalNodeConnectInfo C.CardanoMode
-- connectionInfo (NodeInfo netId socket) =
--   C.LocalNodeConnectInfo
--     (C.CardanoModeParams epochSlots)
--     netId
--     socket
--   where
--     -- This parameter needed only for the Byron era. Since the Byron
--     -- era is over and the parameter has never changed it is ok to
--     -- hardcode this. See comment on `Cardano.Api.ConsensusModeParams` in
--     -- cardano-node.
--     epochSlots = C.EpochSlots 21600

toQueryError :: Show e => e -> NodeQueryError
toQueryError = NodeQueryError . Text.pack . show
