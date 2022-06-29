{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.CollateralEff (
  getCollateralEff,
) where

import BotPlutusInterface.CardanoCLI qualified as CLI
import BotPlutusInterface.Effects (PABEffect, getInMemCollateral, setInMemCollateral)
import BotPlutusInterface.Types (PABConfig)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Either
import Data.Text (Text)
import GHC.Base (Type)
import Ledger (TxOutRef)
import Prelude

-- FIXME: think of better modules structure

getCollateralEff ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Eff effs (Either Text TxOutRef)
getCollateralEff pabConf = do
  -- try to get collateral from memory
  maybeCollateral <- getInMemCollateral @w
  case maybeCollateral of
    Just c -> pure $ Right c
    Nothing -> do
      -- if not in memory, try to create new bu submitting tx
      ethNewCollateral <- makeCollateralEff @w pabConf
      case ethNewCollateral of
        Left e -> pure $ Left ("Failed to make collateral: " <> e)
        Right c -> do
          setInMemCollateral @w c
          pure $ Right c

makeCollateralEff ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Eff effs (Either Text TxOutRef)
makeCollateralEff pabConf = runEitherT $ do
  _ <-
    newEitherT $
      CLI.buildTx @w pabConf awesomeKeysMap mempty awesomeTx
  newEitherT $ CLI.signTx @w pabConf awesomeTx awesomeKeysList
  newEitherT $ CLI.submitTx @w pabConf awesomeTx
  pure awesomeCollateralTxOutRef
  where
    -- FIXME:issue#89: things to consider:
    -- need to make and balance transaction by hand or figure out how to use existing balancing,
    -- maybe pull out `usesScripts` from `addTxCollaterals` and branch `balanceTxIO` depending on it
    awesomeTx = undefined
    awesomeKeysMap = undefined
    awesomeKeysList = undefined
    awesomeCollateralTxOutRef = undefined
