{-# LANGUAGE RankNTypes #-}

{- This is ongoing effort on replacing `cardano-cli` calls with `Cardano.Api` queries, see issues
   https://github.com/mlabs-haskell/bot-plutus-interface/issues/109
   https://github.com/mlabs-haskell/bot-plutus-interface/issues/101
   We decided to provide single replacement for `BotPlutusInterface.CardanoCLI.utxosAt`
   early on to enable inline Datum support from one side and avoid extending
   `cardano-cli` output parser from the other side.
   See https://github.com/mlabs-haskell/bot-plutus-interface/issues/145
-}
module BotPlutusInterface.CardanoNode.Effects (
  utxosAt,
  pparams,
  handleNodeQuery,
  runNodeQuery,
  NodeQuery (..),
  TxOutUtxo,
) where

import BotPlutusInterface.CardanoNode.Query (
  NodeConn,
  NodeQueryError,
  QueryConstraint,
  connectionInfo,
  queryBabbageEra,
  queryInCardanoMode,
  toQueryError,
 )

import BotPlutusInterface.CardanoAPI (
  addressInEraToAny,
 )

import BotPlutusInterface.Types (PABConfig)
import Cardano.Api (AddressInEra, BabbageEra, CardanoMode, EraHistory, TxOut)
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Cardano.Slotting.Time (SystemStart)
import Control.Monad.Freer (Eff, Members, interpret, runM, send, type (~>))
import Control.Monad.Freer.Reader (Reader, runReader)
import Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT, runEitherT)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Tx (TxOutRef)
import Ledger.Tx.CardanoAPI qualified as TxApi
import Prelude

type TxOutUtxo = TxOut CApi.S.CtxUTxO BabbageEra

type NodeQueryResponse a = NodeQuery (Either NodeQueryError a)

{- | 'NodeQuery' effect is used to query local node,
     this is achieved by using 'Cardano.Api'.
-}
data NodeQuery a where
  -- | 'UtxosAt' queries local node to get all the utxos at particular address.
  UtxosAt :: AddressInEra BabbageEra -> NodeQueryResponse (Map TxOutRef TxOutUtxo)
  -- | 'UtxosAtExcluding' queries local node to get all the utxos at particular address
  -- excluding `TxOutRefs`'s specified in `Set`.
  UtxosAtExcluding :: AddressInEra BabbageEra -> Set TxOutRef -> NodeQueryResponse (Map TxOutRef TxOutUtxo)
  -- | 'PParams' queries local node to get it's 'ProtocolParameters'.
  PParams :: NodeQueryResponse CApi.S.ProtocolParameters
  -- | 'UtxosFromTxOutRefs' queries local node to get utxos from a set of tx ins
  UtxosFromTxOutRefs :: Set TxOutRef -> NodeQueryResponse (Map TxOutRef TxOutUtxo)
  -- | 'QuerySystemStart' queries the local node to get the system start
  QuerySystemStart :: NodeQueryResponse SystemStart
  -- | 'QueryEraHistory' queries the local node to get the era history
  QueryEraHistory :: NodeQueryResponse (EraHistory CardanoMode)

utxosAt ::
  forall effs.
  Members '[NodeQuery] effs =>
  AddressInEra BabbageEra ->
  Eff effs (Either NodeQueryError (Map TxOutRef TxOutUtxo))
utxosAt = send . UtxosAt

pparams ::
  forall effs.
  Members '[NodeQuery] effs =>
  Eff effs (Either NodeQueryError CApi.S.ProtocolParameters)
pparams = send PParams

handleNodeQuery ::
  forall effs.
  QueryConstraint effs =>
  Eff (NodeQuery ': effs) ~> Eff effs
handleNodeQuery =
  interpret $ \case
    PParams -> queryBabbageEra CApi.QueryProtocolParameters
    UtxosAt addr -> handleUtxosAt addr
    UtxosAtExcluding addr excluded ->
      let filterOuts = Map.filterWithKey (\oref _ -> not $ oref `Set.member` excluded)
       in fmap filterOuts <$> handleUtxosAt addr
    UtxosFromTxOutRefs outRefs -> handleUtxosFromTxOutRefs outRefs
    QuerySystemStart -> queryInCardanoMode CApi.QuerySystemStart
    QueryEraHistory -> queryInCardanoMode (CApi.QueryEraHistory CApi.CardanoModeIsMultiEra)

handleUtxoQuery ::
  forall effs.
  QueryConstraint effs =>
  CApi.S.QueryInShelleyBasedEra BabbageEra (CApi.UTxO BabbageEra) ->
  EitherT NodeQueryError (Eff effs) (Map TxOutRef TxOutUtxo)
handleUtxoQuery query = do
  (CApi.UTxO result) <- newEitherT $ queryBabbageEra query
  return $ Map.fromList $ fmap (first TxApi.fromCardanoTxIn) $ Map.toList result

handleUtxosFromTxOutRefs ::
  forall effs.
  QueryConstraint effs =>
  Set TxOutRef ->
  Eff effs (Either NodeQueryError (Map TxOutRef TxOutUtxo))
handleUtxosFromTxOutRefs txOutRefs = runEitherT $ do
  txIns <- hoistEither $ first toQueryError $ fmap Set.fromList $ traverse TxApi.toCardanoTxIn $ Set.toList txOutRefs
  handleUtxoQuery $ CApi.QueryUTxO $ CApi.QueryUTxOByTxIn txIns

handleUtxosAt ::
  forall effs.
  QueryConstraint effs =>
  AddressInEra BabbageEra ->
  Eff effs (Either NodeQueryError (Map TxOutRef TxOutUtxo))
handleUtxosAt addr =
  runEitherT $
    handleUtxoQuery $ CApi.QueryUTxO $ CApi.QueryUTxOByAddress $ Set.singleton $ addressInEraToAny addr

-- | 'runNodeQuery' runs executes the 'NodeQuery' effects.
runNodeQuery :: PABConfig -> Eff '[NodeQuery, Reader NodeConn, IO] ~> IO
runNodeQuery conf effs = do
  conn <- connectionInfo conf
  runM $
    runReader conn $
      handleNodeQuery effs
