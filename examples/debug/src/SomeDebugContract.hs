module SomeDebugContract
where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger
  ( Address (Address),
    Extended (Finite),
    Interval (Interval),
    LowerBound (LowerBound),
    POSIXTime (POSIXTime),
    POSIXTimeRange,
    Redeemer (Redeemer),
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    UpperBound (UpperBound),
    Validator,
    always,
    lowerBound,
    scriptAddress,
    strictUpperBound,
    unitDatum,
    validatorHash,
    getCardanoTxId
  )
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts.Validators qualified as Validators
import Plutus.Contract (Contract, submitTx, submitTxConstraintsWith, waitNSlots)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (adaValueOf)
import Plutus.V1.Ledger.Ada qualified as Value
import Plutus.V1.Ledger.Interval (member)
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask
import Tools
import Plutus.V1.Ledger.Api (Credential(PubKeyCredential))
import Debug.Trace (traceM)
import Data.Map qualified as M


utxosAtDebug :: Contract () EmptySchema Text ()
utxosAtDebug = do
  let pkh = pkhFromHash "f433ae2392c0491a9b49acd9ca94033dafba13f8bcd3df5aa840b738"
      addr = Address (PubKeyCredential pkh) Nothing
  utxos <- Contract.utxosAt addr
  traceM $ "UTXOs len: " ++ Hask.show (length $ M.toList utxos ) 
  Hask.undefined