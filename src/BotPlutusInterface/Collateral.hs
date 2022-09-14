module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  filterCollateral,
  mkCollateralTx,
  -- removeCollateralFromPage,
  -- removeCollateralFromMap,
  adjustChainIndexResponse,
  -- filterNodeResponseByQuery,
) where

import BotPlutusInterface.Types (
  CollateralUtxo (CollateralUtxo),
  ContractEnvironment (ceCollateral),
  PABConfig (pcOwnPubKeyHash),
  collateralValue,
  unCollateralVar,
 )
import Cardano.Prelude (Void)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (ChainIndexTxOut, PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Plutus.ChainIndex (Page (pageItems))
import Plutus.ChainIndex.Api (QueryResponse (QueryResponse), TxosResponse (paget), UtxosResponse (page))
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (TxOutRefResponse, TxoSetAtResponse, UnspentTxOutResponse, UnspentTxOutsAtResponse, UtxoSetAtResponse, UtxoSetMembershipResponse))
import Prelude

getInMemCollateral :: forall (w :: Type). ContractEnvironment w -> IO (Maybe CollateralUtxo)
getInMemCollateral = readTVarIO . unCollateralVar . ceCollateral

setInMemCollateral :: forall (w :: Type). ContractEnvironment w -> CollateralUtxo -> IO ()
setInMemCollateral cEnv txOutRef = do
  let cVar = unCollateralVar $ ceCollateral cEnv
  atomically $ writeTVar cVar (Just txOutRef)

mkCollateralTx :: PABConfig -> Either Constraints.MkTxError Constraints.UnbalancedTx
mkCollateralTx pabConf = Constraints.mkTx @Void mempty txc
  where
    txc :: Constraints.TxConstraints Void Void
    txc = Constraints.mustPayToPubKey (PaymentPubKeyHash $ pcOwnPubKeyHash pabConf) (collateralValue pabConf)

filterCollateral :: CollateralUtxo -> [TxOutRef] -> [TxOutRef]
filterCollateral (CollateralUtxo collateralTxOutRef) = filter (/= collateralTxOutRef)

-- | Removes collateral utxo from the UtxoResponse page. Receives `Nothing` if Collateral uninitialized.
removeCollateralFromPage :: Maybe CollateralUtxo -> Page TxOutRef -> Page TxOutRef
removeCollateralFromPage = \case
  Nothing -> id
  Just txOutRef -> \page -> page {pageItems = filterCollateral txOutRef (pageItems page)}

adjustChainIndexResponse :: Maybe CollateralUtxo -> ChainIndexQuery -> ChainIndexResponse -> ChainIndexResponse
adjustChainIndexResponse mc ciQuery ciResponse =
  case mc of
    Nothing -> ciResponse
    Just (CollateralUtxo collateralOref) -> case (ciQuery, ciResponse) of -- FIXME: should `matches` from plutus-apps be used to make sure request matches response?
      (_, UtxoSetAtResponse utxosResp) ->
        let newPage = removeCollateralFromPage mc (page utxosResp)
         in UtxoSetAtResponse $ utxosResp {page = newPage}
      (_, TxoSetAtResponse txosResp) ->
        let newPaget = removeCollateralFromPage mc (paget txosResp)
         in TxoSetAtResponse $ txosResp {paget = newPaget}
      (UnspentTxOutFromRef oref, _) -> if collateralOref == oref then UnspentTxOutResponse Nothing else ciResponse
      (_, UnspentTxOutsAtResponse (QueryResponse refsAndOuts nq)) ->
        let filtered = filter (\v -> fst v /= collateralOref) refsAndOuts
         in UnspentTxOutsAtResponse $ QueryResponse filtered nq
      (TxOutFromRef oref,  _) ->  if collateralOref == oref then TxOutRefResponse Nothing else ciResponse
      (_, UtxoSetMembershipResponse _) -> error "TODO"
      (_, rest) -> rest -- FIXME: would it be better to pattern match everything?

removeCollateralFromMap :: Maybe CollateralUtxo -> Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut
removeCollateralFromMap = \case
  Nothing -> id
  Just (CollateralUtxo collateral) -> Map.filterWithKey (\oref _ -> collateral /= oref)
