{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Module provides the way of building ".raw" transactions with execution budget
 estimated with `Cardano.Api` tools.
-}
module BotPlutusInterface.BodyBuilder (buildAndEstimateBudget) where

import BotPlutusInterface.CardanoCLI qualified as CardanoCLI
import BotPlutusInterface.Effects (PABEffect, estimateBudget)

import BotPlutusInterface.Files (
  DummyPrivKey,
  txFilePath,
 )
import BotPlutusInterface.Types (PABConfig, TxFile (Raw))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Either (firstEitherT, newEitherT, runEitherT)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (ExBudget, Tx, txId)
import Ledger.Crypto (PubKeyHash)
import Prelude

{- TODO: We need to precalculate the tx env defined as
  sysStart
  eraHistory
  pparams
  utxo
from exBudget

make a datatype for this
move the implementation to above the effects system
change the EstimateBudget effect to take the result of whatever function will generate this type
pre-generate it where needed (in balance and submit)
pass it to buildtx for the utxo lookup

notes: may give us a speed boost, as we only need the params once, instead of many times in the balancing loop
potential issues: estimation will need every input in the set, and balancing includes coin selection
we'll need to ensure our utxo lookup includes all inputs, ref inputs, and available funds for coin selection

Another annoyance - the lifted node queries return chain index txouts, ew why
  see if I can get away with changing that, not sure if i can
  if i can't, that means i'll have to implement the tx env stuff as an effect, which is probably fine i guess

maybe this isn't worth it, being explicit, although a hack, might simply be better
  or even some implicit logic, such that if you don't provide the datum for a given input, it'll assume its inline
  if you do, it'll provide it

  or even a specific BPI contract that forces an input to be inline by removing its datum from txData, alongside some extra logic that says
    if you're spending an input, dont provide a datum, and havent explicitly stated the input has inline datum, error out
    
  seems like more strain on the user, more esoteric behaviour of BPI
  the "right" solution is for BPI to work it all out, but that could take a while

http://localhost:8070/file/nix/store/hg0h3096cqprcr6z75f57bhnqhxny54f-plutus-ledger-lib-plutus-ledger-1.0.0-haddock-doc/share/doc/plutus-ledger/html/Ledger-Tx-CardanoAPI-Internal.html#v:toCardanoTxIn

we can lift all the queries above the effect
  we would need to convert ChainIndexTxOut -> TxOut CtxUTxO BabbageEra
    specifically the datum type is Maybe (Hash, Maybe datum)
      first maybe = is there a datum
      inner maybe = is it inlined
    For TxOutDatum, given we have CtxUTxO, only the 3 obvious constructors are usable
  
  this can be build now on top of the existing ChainIndexQuery effect (no new effect)
  as for the other data - era, start, blah

  The utxosAt via node query returns ChainIndexTxOut, but converts from TxOut internally
  we can remove this conversion
    only uses I can see convert it straight back anyway
  then we can implement this logic properly
  as for other queries, params has an effect, the other 2 will nneed additional effects in CardanoNode.Effects
    then we're good i think? no additional main effects
    should be faster

  steps:
    - change the node utxos queries to return TxOut over ChainIndexTxOut, fix what that breaks
    - implement a function that gets full utxo data from a set of txins via node
    - implement additional node effects for the other pieces of data we need
    - implement another function that uses both above, and combines them into some data type
    - implement a helper function that takes above txEstInfo and a Set TxOutRef (of new refs) and adds them to the utxo mapping
    - move the lookup logic out of estimate, maybe also drop the node reader, pass the above data type into the effect
    - change balance to now take this data type, and update it as needed (perhaps use state)
    - change buildandestimate to work out prelim value and pass it down

data CardanoInfo = CardanoInfo
  { params
  , startTime
  , eraHistory
  }

getInfo :: Eff effs CardanoInfo
getUtxos :: Set TxOutRef -> Eff effs (Map TxOutRef TxOut)

data EstimationContext = EstimationContext CardanoInfo (Map TxOutRef TxOut)

getEstimationContext :: Set TxOutRef -> Eff effs EstimationContext

unionEstimationContext :: Set TxOutRef -> EstimationContext -> Eff effs EstimationContext

-}

{- | Build and save raw transaction (transaction body) with estimated execution budgets using `CardanoCLI`.
 It builds first transaction body with 0 budget for all spending inputs and minting policies,
 then uses body of this transaction to estimate execution budget
 and build final body with budget set.
-}
buildAndEstimateBudget ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  Tx ->
  Eff effs (Either Text ExBudget)
buildAndEstimateBudget pabConf privKeys tx = runEitherT $
  buildDraftTxBody
    >> estimateBudgetByDraftBody (Text.unpack $ txFilePath pabConf "raw" (txId tx))
    >>= buildBodyUsingEstimatedBudget
  where
    buildDraftTxBody = newEitherT $ CardanoCLI.buildTx @w pabConf privKeys mempty tx

    estimateBudgetByDraftBody path =
      firstEitherT toText . newEitherT $ estimateBudget @w (Raw path)

    buildBodyUsingEstimatedBudget exBudget =
      newEitherT $
        CardanoCLI.buildTx @w
          pabConf
          privKeys
          exBudget
          tx

    toText = Text.pack . show
