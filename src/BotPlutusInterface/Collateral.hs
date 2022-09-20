module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  mkCollateralTx,
  withCollateralHandling,
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
import Control.Monad (unless)
import Data.Kind (Type)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef)
import Ledger.Constraints qualified as Constraints
import Plutus.ChainIndex (Page (pageItems))
import Plutus.ChainIndex.Api (
  IsUtxoResponse (IsUtxoResponse),
  QueryResponse (QueryResponse),
  TxosResponse (paget),
  UtxosResponse (page),
 )
import Plutus.Contract.Effects (
  ChainIndexQuery (..),
  ChainIndexResponse (
    TxOutRefResponse,
    TxoSetAtResponse,
    UnspentTxOutResponse,
    UnspentTxOutsAtResponse,
    UtxoSetAtResponse,
    UtxoSetMembershipResponse,
    UtxoSetWithCurrencyResponse
  ),
  PABReq (ChainIndexQueryReq),
  PABResp (ChainIndexQueryResp),
  matches,
 )
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

-- | Middleware to run `chain-index` queries and filter out collateral output from response.
withCollateralHandling ::
  Monad m =>
  Maybe CollateralUtxo ->
  (ChainIndexQuery -> m ChainIndexResponse) ->
  ChainIndexQuery ->
  m ChainIndexResponse
withCollateralHandling mCollateral runChainIndexQuery = \query -> do
  response <-
    adjustChainIndexResponse mCollateral query
      <$> runChainIndexQuery query
  ensureMatches query response
  pure response
  where
    ensureMatches query result =
      unless (matches (ChainIndexQueryReq query) (ChainIndexQueryResp result)) $
        error $
          mconcat
            [ "Chain-index request doesn't match response."
            , "\nRequest: " ++ show query
            , "\nResponse:" ++ show result
            ]

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
      (_, UtxoSetWithCurrencyResponse utxosResp) ->
        let newPage = removeCollateralFromPage mc (page utxosResp)
         in UtxoSetWithCurrencyResponse $ utxosResp {page = newPage}
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

-- | Removes collateral utxo from the UtxoResponse page. Receives `Nothing` if Collateral uninitialized.
removeCollateralFromPage :: Maybe CollateralUtxo -> Page TxOutRef -> Page TxOutRef
removeCollateralFromPage = \case
  Nothing -> id
  Just txOutRef -> \page' -> page' {pageItems = filterCollateral txOutRef (pageItems page')}

filterCollateral :: CollateralUtxo -> [TxOutRef] -> [TxOutRef]
filterCollateral (CollateralUtxo collateralTxOutRef) = filter (/= collateralTxOutRef)
