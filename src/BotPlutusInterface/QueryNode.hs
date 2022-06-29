{-# LANGUAGE ViewPatterns #-}

-- | Several query functions to query local node
module BotPlutusInterface.QueryNode (
  NodeInfo (..),
  queryProtocolParams,
  querySystemStart,
  queryEraHistory,
  queryOutsByInputs,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Prelude

{- | Error returned in case any error happened querying local node
 (wraps whatever received in `Text`)
-}
data NodeQueryError
  = NodeQueryError Text
  deriving stock (Eq, Show)

data NodeInfo = NodeInfo
  { niNetworkId :: C.NetworkId
  , niSocket :: FilePath
  }

queryProtocolParams :: NodeInfo -> IO (Either NodeQueryError ProtocolParameters)
queryProtocolParams (connectionInfo -> cInfo) =
  flattenQueryResult <$> C.queryNodeLocalState cInfo Nothing query
  where
    query =
      C.QueryInEra C.BabbageEraInCardanoMode $
        C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters

querySystemStart :: NodeInfo -> IO (Either NodeQueryError SystemStart)
querySystemStart (connectionInfo -> cInfo) =
  left toQueryError
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      C.QuerySystemStart

queryEraHistory :: NodeInfo -> IO (Either NodeQueryError (C.EraHistory C.CardanoMode))
queryEraHistory (connectionInfo -> cInfo) =
  left toQueryError
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      (C.QueryEraHistory C.CardanoModeIsMultiEra)

queryOutsByInputs :: NodeInfo -> [C.TxIn] -> IO (Either NodeQueryError (C.UTxO C.BabbageEra))
queryOutsByInputs (connectionInfo -> cInfo) ins =
  flattenQueryResult
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      query
  where
    query =
      C.QueryInEra C.BabbageEraInCardanoMode $
        C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage $
          C.QueryUTxO (C.QueryUTxOByTxIn (Set.fromList ins))

flattenQueryResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either NodeQueryError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ NodeQueryError (pack $ show err)

connectionInfo :: NodeInfo -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfo (NodeInfo netId socket) =
  C.LocalNodeConnectInfo
    ( C.CardanoModeParams
        (C.EpochSlots 21600) -- TODO: this probably should be settable somehow?
    )
    netId
    socket

toQueryError :: Show e => e -> NodeQueryError
toQueryError = NodeQueryError . pack . show
