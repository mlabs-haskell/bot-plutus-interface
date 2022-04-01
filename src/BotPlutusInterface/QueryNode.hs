{-# LANGUAGE ViewPatterns #-}

module BotPlutusInterface.QueryNode where

import Cardano.Api qualified as C
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Data.Text (Text, pack)
import Data.Set qualified as Set
import Prelude

data NodeQueryError
  = NodeQueryError Text
  deriving stock (Eq, Show)

data NodeInfo = NodeInfo
  { niNetworkId :: C.NetworkId,
    niSocket :: FilePath
  }

protocolParams :: NodeInfo -> IO (Either NodeQueryError ProtocolParameters)
protocolParams (connectionInfo -> cInfo) =
  flattenQueryResult <$> C.queryNodeLocalState cInfo Nothing query
  where
    query =
      C.QueryInEra C.AlonzoEraInCardanoMode $
        C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo C.QueryProtocolParameters

systemStart :: NodeInfo -> IO (Either NodeQueryError SystemStart)
systemStart (connectionInfo -> cInfo) =
  left toQueryError
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      C.QuerySystemStart

eraHistory :: NodeInfo -> IO (Either NodeQueryError (C.EraHistory C.CardanoMode))
eraHistory (connectionInfo -> cInfo) =
  left toQueryError
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      (C.QueryEraHistory C.CardanoModeIsMultiEra)

outsByInputs :: NodeInfo -> [C.TxIn] -> IO (Either NodeQueryError (C.UTxO C.AlonzoEra))
outsByInputs (connectionInfo -> cInfo) ins =
  flattenQueryResult
    <$> C.queryNodeLocalState
      cInfo
      Nothing
      query
  where
    query = 
      C.QueryInEra C.AlonzoEraInCardanoMode $
        C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo $
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
    (C.CardanoModeParams (C.EpochSlots 21600)) -- TODO: is this number correct?
    netId
    socket

toQueryError :: Show e => e -> NodeQueryError
toQueryError = NodeQueryError . pack . show
