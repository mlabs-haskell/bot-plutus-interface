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
) where

import BotPlutusInterface.CardanoNode.Query (
  NodeConn,
  NodeQueryError,
  QueryConstraint,
  connectionInfo,
  queryBabbageEra,
  toQueryError,
 )

import BotPlutusInterface.CardanoAPI (
  addressInEraToAny,
  fromCardanoTxOut,
 )

import BotPlutusInterface.Types (PABConfig)
import Cardano.Api (LocalNodeConnectInfo (..))
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Control.Lens (folded, to, (^..))
import Control.Monad.Freer (Eff, Members, interpret, runM, send, type (~>))
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Address (Address)
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Tx.CardanoAPI qualified as TxApi
import Plutus.V2.Ledger.Tx qualified as V2
import Prelude

{- | 'NodeQuery' effect is used to query local node,
     this is achieved by using 'Cardano.Api'.
-}
data NodeQuery a where
  -- | 'UtxosAt' queries local node to get all the utxos at particular address.
  UtxosAt :: Address -> NodeQuery (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
  -- | 'PParams' queries local node to get it's 'ProtocolParameters'.
  PParams :: NodeQuery (Either NodeQueryError CApi.S.ProtocolParameters)

utxosAt ::
  forall effs.
  Members '[NodeQuery] effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
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
    UtxosAt addr -> handleUtxosAt addr
    PParams -> queryBabbageEra CApi.QueryProtocolParameters

handleUtxosAt ::
  forall effs.
  QueryConstraint effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
handleUtxosAt addr = runEitherT $ do
  conn <- lift $ ask @NodeConn

  caddr <-
    firstEitherT toQueryError $
      hoistEither $
        TxApi.toCardanoAddressInEra (localNodeNetworkId conn) addr

  let query :: CApi.QueryInShelleyBasedEra era (CApi.UTxO era)
      query = CApi.QueryUTxO $ CApi.QueryUTxOByAddress $ Set.singleton $ addressInEraToAny caddr

  (CApi.UTxO result) <- newEitherT $ queryBabbageEra query

  chainIndexTxOuts <-
    firstEitherT toQueryError $
      hoistEither $
        sequenceA $
          result ^.. folded . to fromCardanoTxOut

  let txOutRefs :: [V2.TxOutRef]
      txOutRefs = TxApi.fromCardanoTxIn <$> Map.keys result

  return $ Map.fromList $ zip txOutRefs chainIndexTxOuts

-- | 'runNodeQuery' runs executes the 'NodeQuery' effects.
runNodeQuery :: PABConfig -> Eff '[NodeQuery, Reader NodeConn, IO] ~> IO
runNodeQuery conf effs = do
  conn <- connectionInfo conf
  runM $
    runReader conn $
      handleNodeQuery effs
