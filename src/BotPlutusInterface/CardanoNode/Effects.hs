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
  minUtxo,
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
import Cardano.Ledger.Shelley.API.Wallet (
  CLI (evaluateMinLovelaceOutput),
 )
import Control.Lens (folded, to, (^..))
import Control.Monad.Freer (Eff, Members, interpret, runM, send, type (~>))
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address)
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Tx.CardanoAPI qualified as TxApi
import Ledger.Validation (Coin (Coin))
import Plutus.V2.Ledger.Tx qualified as V2
import Prelude

data NodeQuery a where
  UtxosAt :: Address -> NodeQuery (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
  PParams :: NodeQuery (Either NodeQueryError CApi.S.ProtocolParameters)
  MinUtxo :: Ledger.TxOut -> NodeQuery (Either NodeQueryError Ledger.TxOut)

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

minUtxo ::
  forall effs.
  Members '[NodeQuery] effs =>
  Ledger.TxOut ->
  Eff effs (Either NodeQueryError Ledger.TxOut)
minUtxo = send . MinUtxo

handleNodeQuery ::
  forall effs.
  QueryConstraint effs =>
  Eff (NodeQuery ': effs) ~> Eff effs
handleNodeQuery =
  interpret $ \case
    UtxosAt addr -> handleUtxosAt addr
    PParams -> queryBabbageEra CApi.QueryProtocolParameters
    MinUtxo txout -> handleMinUtxo txout

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

handleMinUtxo ::
  forall effs.
  QueryConstraint effs =>
  Ledger.TxOut ->
  Eff effs (Either NodeQueryError Ledger.TxOut)
handleMinUtxo txout = runEitherT $ do
  conn <- lift $ ask @NodeConn

  params <- newEitherT $ queryBabbageEra CApi.QueryProtocolParameters

  let pparamsInEra = CApi.toLedgerPParams CApi.ShelleyBasedEraBabbage params
      netId = localNodeNetworkId conn

  ctxout <-
    firstEitherT toQueryError $
      hoistEither $
        TxApi.toCardanoTxOut netId TxApi.toCardanoTxOutDatumHash txout

  let (Coin minTxOut) =
        evaluateMinLovelaceOutput pparamsInEra $
          CApi.S.toShelleyTxOut CApi.ShelleyBasedEraBabbage ctxout

      missingLovelace = Ada.lovelaceOf minTxOut - Ada.fromValue (Ledger.txOutValue txout)

  if missingLovelace > 0
    then
      newEitherT $
        handleMinUtxo (txout {Ledger.txOutValue = Ledger.txOutValue txout <> Ada.toValue missingLovelace})
    else return txout

runNodeQuery :: PABConfig -> Eff '[NodeQuery, Reader NodeConn, IO] ~> IO
runNodeQuery conf effs = do
  conn <- connectionInfo conf
  runM $
    runReader conn $
      handleNodeQuery effs
