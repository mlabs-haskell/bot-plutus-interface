{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Module provides the way of building ".raw" transactions with execution budget
 estimated with `Cardano.Api` tools.
-}
module BotPlutusInterface.BodyBuilder (buildRaw) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, estimateBudget)
import BotPlutusInterface.Files (
  DummyPrivKey,
 )
import BotPlutusInterface.Types (PABConfig, TxFile (Raw))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Either (firstEitherT, newEitherT, runEitherT)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (ExBudget, Tx)
import Ledger.Crypto (PubKeyHash)
import Prelude
import BotPlutusInterface.Estimate (getMaxBudgets)

{- | Build and save raw transaction (transaction body) with estimated execution budgets using `CardanoCLI`.
 It builds first transaction body with 0 budget for all spending inputs and minting policies,
 then uses body of this transaction to estimate execution budget
 and build final body with budget set.
-}
buildRaw ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  Tx ->
  Eff effs (Either Text ExBudget)
buildRaw pabConf privKeys tx = runEitherT $ do
  buildDraftTxBody
    >>= estimateBudgetByDraftBody
    >>= buildBodyUsingEstimatedBudget
  where
    buildDraftTxBody = newEitherT $ CardanoCLI.buildDraftTx @w pabConf privKeys tx

    estimateBudgetByDraftBody path =
      firstEitherT toText . newEitherT $ estimateBudget @w (Raw path)

    buildBodyUsingEstimatedBudget exBudget =
      newEitherT $
        CardanoCLI.buildTx @w
          pabConf
          privKeys
          (getMaxBudgets exBudget)
          tx

    toText = Text.pack . show
