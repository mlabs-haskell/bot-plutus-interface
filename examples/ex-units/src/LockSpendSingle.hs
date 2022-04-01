module LockSpendSingle (lockThenSpendSingle) where

import Prelude

import Data.Map qualified as Map
import Data.Text (Text)
import Ledger
    ( CardanoTx,
      Address,
      ScriptContext,
      TxId,
      Validator,
      getCardanoTxId,
      scriptAddress,
      unitDatum,
      unitRedeemer,
      validatorHash )
import Ledger.Constraints qualified as Constraints
import Plutus.Contract
    ( Contract, submitTx, awaitTxConfirmed, submitTxConstraintsWith )
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Ledger.Typed.Scripts.Validators qualified as Validators
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified

lockThenSpendSingle :: Contract () EmptySchema Text (TxId, CardanoTx)
lockThenSpendSingle =
  lockAtScript >> Contract.waitNSlots 1 >> spendFromScript


lockAtScript :: Contract () EmptySchema Text (TxId, CardanoTx)
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Value.adaValueOf 10)
  tx <- submitTx constr
  awaitTxConfirmed $ getCardanoTxId tx
  pure (getCardanoTxId tx, tx)

spendFromScript :: Contract () EmptySchema Text (TxId, CardanoTx)
spendFromScript = do
  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [] -> Contract.throwError "No UTxOs at script address"
    (oref, _) : _ -> spendUtxo oref utxos
  where
    spendUtxo oref utxos = do
      let txc = Constraints.mustSpendScriptOutput oref unitRedeemer
          lookups =
            Constraints.unspentOutputs (Map.fromList utxos)
              <> Constraints.otherScript validator
      tx <- submitTxConstraintsWith @TestLockSpend lookups txc
      awaitTxConfirmed $ getCardanoTxId tx
      pure (getCardanoTxId tx, tx)


-- Always true Script and spending contract

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

data TestLockSpend

instance Validators.ValidatorTypes TestLockSpend where
  type DatumType TestLockSpend = ()
  type RedeemerType TestLockSpend = ()

typedValidator :: Validators.TypedValidator TestLockSpend
typedValidator =
  Validators.mkTypedValidator @TestLockSpend
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @() @()

validator :: Validator
validator = Validators.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptAddress validator
