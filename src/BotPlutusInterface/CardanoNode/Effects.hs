module BotPlutusInterface.CardanoNode.Effects (
  utxosAt,
  handleQueryNode,
) where

import BotPlutusInterface.CardanoNode.Query (NodeConn, NodeQueryError, QueryConstraint, queryBabbageEra, toQueryError)
import Cardano.Api (LocalNodeConnectInfo (..))
import Cardano.Api qualified as CApi
import Control.Lens (folded, to, (^..))
import Control.Monad.Freer (Eff, Members, interpret, send, type (~>))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified
import Ledger.Address (Address)
import Ledger.Tx.CardanoAPI qualified as TxApi
import Plutus.ChainIndex.Types (ChainIndexTxOut (..))
import Plutus.Contract.CardanoAPI qualified as TxApi
import Plutus.V2.Ledger.Tx qualified as V2
import PlutusTx.Prelude qualified as PlutusTx
import Prelude

data QueryNode a where
  UtxosAt :: CApi.AddressAny -> QueryNode (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))

utxosAt ::
  forall effs.
  Members '[QueryNode, Reader NodeConn] effs =>
  Address ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
utxosAt addr = runEitherT $ do
  info <- lift $ ask @NodeConn @effs

  caddr <-
    firstEitherT toQueryError $
      hoistEither $
        TxApi.toCardanoAddressInEra (localNodeNetworkId info) addr

  newEitherT $ send @QueryNode @effs (UtxosAt $ addressInEraToAny caddr)

handleQueryNode ::
  forall effs.
  QueryConstraint effs =>
  Eff (QueryNode ': effs) ~> Eff effs
handleQueryNode =
  interpret $ \case
    UtxosAt addr -> handleUtxosAt addr

handleUtxosAt ::
  forall effs.
  QueryConstraint effs =>
  CApi.AddressAny ->
  Eff effs (Either NodeQueryError (Map V2.TxOutRef ChainIndexTxOut))
handleUtxosAt addr = runEitherT $ do
  let query :: CApi.QueryInShelleyBasedEra era (CApi.UTxO era)
      query = CApi.QueryUTxO $ CApi.QueryUTxOByAddress $ Set.singleton addr

  (CApi.UTxO result) <- newEitherT $ queryBabbageEra query

  chainIndexTxOuts <-
    firstEitherT toQueryError $
      hoistEither $
        sequenceA $
          result ^.. folded . to fromCardanoTxOut

  let txOutRefs :: [V2.TxOutRef]
      txOutRefs = TxApi.fromCardanoTxIn <$> Map.keys result

  return $ Map.fromList $ zip txOutRefs chainIndexTxOuts

----------------------
-- Conversions
----------------------

fromCardanoTxOut :: CApi.TxOut CApi.CtxUTxO CApi.BabbageEra -> Either TxApi.FromCardanoError ChainIndexTxOut
fromCardanoTxOut (CApi.TxOut addr val datum refScript) =
  ChainIndexTxOut
    <$> TxApi.fromCardanoAddressInEra addr
    <*> pure (TxApi.fromCardanoValue $ CApi.txOutValueToValue val)
    <*> pure (fromCardanoTxOutDatum datum)
    <*> pure (TxApi.fromCardanoTxOutRefScript refScript)

fromCardanoTxOutDatum :: CApi.TxOutDatum CApi.CtxUTxO CApi.BabbageEra -> V2.OutputDatum
fromCardanoTxOutDatum CApi.TxOutDatumNone = V2.NoOutputDatum
fromCardanoTxOutDatum (CApi.TxOutDatumHash _ h) = V2.OutputDatumHash $ Ledger.DatumHash $ PlutusTx.toBuiltin (CApi.serialiseToRawBytes h)
fromCardanoTxOutDatum (CApi.TxOutDatumInline _ d) = V2.OutputDatum $ Ledger.Datum $ TxApi.fromCardanoScriptData d

addressInEraToAny :: CApi.AddressInEra CApi.BabbageEra -> CApi.AddressAny
addressInEraToAny (CApi.AddressInEra CApi.ByronAddressInAnyEra a) = CApi.AddressByron a
addressInEraToAny (CApi.AddressInEra (CApi.ShelleyAddressInEra _) a) = CApi.AddressShelley a
