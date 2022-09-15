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
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Plutus.ChainIndex (Page (pageItems))
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), QueryResponse (QueryResponse), TxosResponse (paget), UtxosResponse (page))
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
  Just txOutRef -> \page' -> page' {pageItems = filterCollateral txOutRef (pageItems page')}

adjustChainIndexResponse :: Maybe CollateralUtxo -> ChainIndexQuery -> ChainIndexResponse -> ChainIndexResponse
adjustChainIndexResponse mc ciQuery ciResponse =
  case mc of
    Nothing -> ciResponse
    Just (CollateralUtxo collateralOref) -> case (ciQuery, ciResponse) of
      -- adjustment based on response
      (_, UtxoSetAtResponse utxosResp) ->
        let newPage = removeCollateralFromPage mc (page utxosResp)
         in UtxoSetAtResponse $ utxosResp {page = newPage}
      (_, TxoSetAtResponse txosResp) ->
        let newPaget = removeCollateralFromPage mc (paget txosResp)
         in TxoSetAtResponse $ txosResp {paget = newPaget}
      (_, UnspentTxOutsAtResponse (QueryResponse refsAndOuts nq)) ->
        let filtered = filter (\v -> fst v /= collateralOref) refsAndOuts
         in UnspentTxOutsAtResponse $ QueryResponse filtered nq
      -- adjustment based on request
      (UtxoSetMembership oref, UtxoSetMembershipResponse (IsUtxoResponse ct isU)) ->
        UtxoSetMembershipResponse $
          IsUtxoResponse ct $
            oref /= collateralOref && isU
      (TxOutFromRef oref, TxOutRefResponse _) ->
        if collateralOref == oref
          then TxOutRefResponse Nothing
          else ciResponse
      (UnspentTxOutFromRef oref, UnspentTxOutResponse _) ->
        if collateralOref == oref
          then UnspentTxOutResponse Nothing
          else ciResponse
      -- all other cases
      (_, rest) -> rest
