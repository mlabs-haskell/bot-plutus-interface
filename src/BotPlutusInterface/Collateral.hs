module BotPlutusInterface.Collateral (
  getInMemCollateral,
  setInMemCollateral,
  filterCollateral,
  mkCollateralTx,
  removeCollateralFromPage,
  removeCollateralFromMap,
) where

import BotPlutusInterface.Types (ContractEnvironment (ceCollateral), PABConfig (pcOwnPubKeyHash), collateralValue, unCollateralVar, CollateralUtxo (CollateralUtxo))
import Cardano.Prelude (Void)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxOutRef, ChainIndexTxOut)
import Ledger.Constraints qualified as Constraints
import Prelude
import Plutus.ChainIndex (Page(..))
import Data.Map (Map)
import qualified Data.Map as Map

getInMemCollateral :: ContractEnvironment w -> IO (Maybe CollateralUtxo)
getInMemCollateral = readTVarIO . unCollateralVar . ceCollateral

setInMemCollateral :: ContractEnvironment w -> CollateralUtxo -> IO ()
setInMemCollateral cEnv txOutRef = do
  let cVar = unCollateralVar $ ceCollateral cEnv
  atomically $ modifyTVar' cVar (const (Just txOutRef))

mkCollateralTx :: PABConfig -> Either Constraints.MkTxError Constraints.UnbalancedTx
mkCollateralTx pabConf = Constraints.mkTx @Void mempty txc
  where
    txc = Constraints.mustPayToPubKey (PaymentPubKeyHash $ pcOwnPubKeyHash pabConf) (collateralValue pabConf)

filterCollateral :: CollateralUtxo -> [TxOutRef] -> [TxOutRef]
filterCollateral (CollateralUtxo collateralTxOutRef) = filter (/= collateralTxOutRef)

-- | Removes collateral utxo from the UtxoResponse page. Receives `Nothing` if Collateral uninitialized.
removeCollateralFromPage :: Maybe CollateralUtxo -> Page TxOutRef -> Page TxOutRef
removeCollateralFromPage = \case
  Nothing -> id
  (Just txOutRef) -> \page -> page {pageItems = filterCollateral txOutRef (pageItems page)}

removeCollateralFromMap :: Maybe CollateralUtxo -> Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut
removeCollateralFromMap = \case
  Nothing -> id
  Just (CollateralUtxo collateral) -> Map.filterWithKey (\oref _ -> collateral /= oref)