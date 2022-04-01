{-# LANGUAGE AllowAmbiguousTypes #-}
module BotPlutusInterface.BodyBuilder (buildRaw) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, estimateBudget)
import BotPlutusInterface.Files
  ( DummyPrivKey,
  )
import BotPlutusInterface.Estimate (TxFile (Raw), getMaxBudgets)
import BotPlutusInterface.Types (PABConfig)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Either (firstEitherT, newEitherT, runEitherT)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (ExBudget, Tx)
import Ledger.Crypto (PubKeyHash)
import Prelude


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
