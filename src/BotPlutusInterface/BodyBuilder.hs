{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Module provides the way of building ".raw" transactions with execution budget
 estimated with `Cardano.Api` tools.
-}
module BotPlutusInterface.BodyBuilder (buildAndEstimateBudget, runInEstimationEffect) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, estimateBudget, getEstimationContext)

import BotPlutusInterface.Files (
  DummyPrivKey,
  txFilePath,
 )
import BotPlutusInterface.Types (EstimationContext, PABConfig, TxBudget, TxFile (Raw))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State (State, evalState, get)
import Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT, newEitherT)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (Tx (txInputs), TxInput (txInputRef), txId)
import Ledger.Crypto (PubKeyHash)
import Prelude

textShow :: forall (a :: Type). Show a => a -> Text
textShow = Text.pack . show

{- | We pull out the context needed for estimation and CLI tx building into a state effect.
  Getting this state is in itself effectful, and as such, we define this helper function that lifts a computation into the state we need,
  handling the query and error logic for us
  buildAndEstimateBudget, buildTx and balanceTxIOEstimationContext should all be run within this wrapper
-}
runInEstimationEffect ::
  forall (w :: Type) (a :: Type) (e :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Tx ->
  (Text -> e) ->
  EitherT e (Eff (State EstimationContext ': effs)) a ->
  EitherT e (Eff effs) a
runInEstimationEffect tx toErr comp = do
  context <- firstEitherT (toErr . textShow) $ newEitherT $ getEstimationContext @w $ Set.fromList $ txInputRef <$> txInputs tx
  mapEitherT (evalState context) comp

{- | Build and save raw transaction (transaction body) with estimated execution budgets using `CardanoCLI`.
 It builds first transaction body with 0 budget for all spending inputs and minting policies,
 then uses body of this transaction to estimate execution budget
 and build final body with budget set.
-}
buildAndEstimateBudget ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ( Member (PABEffect w) effs
  , Member (State EstimationContext) effs
  ) =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  Tx ->
  EitherT Text (Eff effs) TxBudget
buildAndEstimateBudget pabConf privKeys tx =
  buildDraftTxBody
    >> estimateBudgetByDraftBody (Text.unpack $ txFilePath pabConf "raw" (txId tx))
    >>= buildBodyUsingEstimatedBudget
  where
    buildDraftTxBody = newEitherT $ CardanoCLI.buildTx @w pabConf privKeys mempty tx

    estimateBudgetByDraftBody path =
      firstEitherT textShow . newEitherT $ get >>= flip (estimateBudget @w) (Raw path)

    buildBodyUsingEstimatedBudget txBudget =
      fmap (const txBudget) $
        newEitherT $
          CardanoCLI.buildTx @w
            pabConf
            privKeys
            txBudget
            tx
