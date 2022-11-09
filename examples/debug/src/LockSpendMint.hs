module Spec.TestContract.LockSpendMint (lockThenSpend) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import Control.Monad (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Void (Void)
import Ledger
  ( Address,
    CardanoTx,
    ChainIndexTxOut,
    CurrencySymbol,
    PaymentPubKeyHash (PaymentPubKeyHash),
    ScriptContext (scriptContextTxInfo),
    TxId,
    TxInfo (txInfoMint),
    TxOutRef,
    getCardanoTxId,
    scriptHashAddress,
  )
import Ledger.Ada (adaValueOf)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (TypedValidator, Validator, ValidatorTypes, mkUntypedMintingPolicy)
import Ledger.Typed.Scripts qualified as TypedScripts
import Ledger.Value (flattenValue, tokenName)
import Plutus.Contract (Contract, awaitTxConfirmed)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP
import Prelude
import qualified Data.Text as T
import Cardano.Internal.Compat (fromRight)

lockThenSpend :: Contract () EmptySchema Text [(TxOutRef, ChainIndexTxOut)]
lockThenSpend = do
  -- testContract1
  _ <- lockAtScript
  wait 1
  _ <- spendFromScript
  -- wait 1
  addr <- NonEmpty.head <$> Contract.ownAddresses
  Map.toList <$> Contract.utxosAt addr
  where
    wait = void . Contract.waitNSlots

debugPayValue = adaValueOf 20
testContract1 :: Contract () EmptySchema Text ()
testContract1 = do
  ownPkh <- Contract.ownFirstPaymentPubKeyHash
  ownAddr <- NonEmpty.head <$> Contract.ownAddresses
  ownUtxos <- Contract.utxosAt ownAddr

  let txc = 
        Constraints.mustPayToPubKey ownPkh (adaValueOf 22)
        <> Constraints.mustPayToPubKey ownPkh (adaValueOf 11)
        <> Constraints.mustPayToPubKey ownPkh (adaValueOf 33)

      lkps = Constraints.unspentOutputs ownUtxos

  tx <- submitBpiTxConstraintsWith @TestLockSpend lkps txc []
  awaitTxConfirmed $ getCardanoTxId tx
  pure ()

lockAtScript :: Contract () EmptySchema Text (TxId, CardanoTx)
lockAtScript = do
  let constr2 =
        Constraints.mustPayToOtherScriptWithDatumInTx -- WARN: mustPayToOtherScript doesn't work with DatumNotFound
          (ScriptUtils.validatorHash $ validator2 2)
          Scripts.unitDatum
          debugPayValue
  tx <- submitBpiTxConstraintsWith @Void mempty constr2 []
  awaitTxConfirmed $ getCardanoTxId tx
  pure (getCardanoTxId tx, tx)

spendFromScript :: Contract () EmptySchema Text (TxId, CardanoTx)
spendFromScript = do
  ownPkh <- Contract.ownFirstPaymentPubKeyHash
  utxos2 <- Map.toList <$> Contract.utxosAt (validatorAddr2 2)
  case utxos2 of
    [] -> Contract.throwError "No UTxOs at script address"
    (oref2, _) : _ -> spendUtxo oref2 utxos2 ownPkh
  where
    spendUtxo oref2 utxos2 ownPkh = do
      let txc2 =
            Constraints.mustSpendScriptOutput oref2 Scripts.unitRedeemer
              -- <> Constraints.mustPayToPubKey
              --   (PaymentPubKeyHash "72cae61f85ed97fb0e7703d9fec382e4973bf47ea2ac9335cab1e3fe")
              --   (adaValueOf 33)
          lookups2 =
            Constraints.unspentOutputs (Map.fromList utxos2)
              <> Constraints.plutusV1OtherScript (validator2 2)
      Contract.logInfo @String $ "@@Script oref: " <> show oref2

      params <- Contract.getParams
      let result = fromRight (error "lol") $ Constraints.mkTxWithParams params lookups2 txc2

      addr <- NonEmpty.head <$> Contract.ownAddresses
      ownOrefs <- Map.keys <$> Contract.utxosAt addr

      Contract.logDebug $ 
        "\nScript oref to spend:\n  " <> show (pretty oref2)
        <> "\nOwn orefs:\n  " <> show (pretty ownOrefs) 
        <>"\nRresult:\n  " <> show (pretty result)
      tx <- submitBpiTxConstraintsWith @TestLockSpend lookups2 txc2 []
      awaitTxConfirmed $ getCardanoTxId tx
      pure (getCardanoTxId tx, tx)

-- Always true Script and spending contract

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = PP.traceIfFalse "validator 1 error" True

data TestLockSpend

instance ValidatorTypes TestLockSpend where
  type DatumType TestLockSpend = ()
  type RedeemerType TestLockSpend = ()

typedValidator :: TypedValidator TestLockSpend
typedValidator =
  TypedScripts.mkTypedValidator @TestLockSpend
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator @() @()

validator :: Validator
validator = TypedScripts.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress $ ScriptUtils.validatorHash validator

{-# INLINEABLE mkValidator2 #-}
mkValidator2 :: Integer -> () -> () -> ScriptContext -> Bool
mkValidator2 i _ _ _ =
  if i PP./= 1
    then PP.traceIfFalse "looooooooooooong" check
    else PP.traceIfFalse "short" check
  where
    someWork = PP.sort $ PP.reverse [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Integer]
    check = PP.length someWork PP.== 10

data TestLockSpend2

instance ValidatorTypes TestLockSpend2 where
  type DatumType TestLockSpend2 = ()
  type RedeemerType TestLockSpend2 = ()

typedValidator2 :: Integer -> TypedValidator TestLockSpend
typedValidator2 uid =
  TypedScripts.mkTypedValidator @TestLockSpend
    ($$(PlutusTx.compile [||mkValidator2||]) `PlutusTx.applyCode` PlutusTx.liftCode uid)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator @() @()

validator2 :: Integer -> Validator
validator2 = TypedScripts.validatorScript . typedValidator2

validatorAddr2 :: Integer -> Address
validatorAddr2 = scriptHashAddress . ScriptUtils.validatorHash . validator2

-- minting policy
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ ctx =
  PP.traceIfFalse "Let me mint" check
  where
    info = scriptContextTxInfo ctx
    check =
      PP.length (flattenValue PP.$ txInfoMint info) PP.== 1
        PP.&& PP.length someWork PP.== 10

    someWork = PP.sort [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: [Integer]

mintingPolicy :: TypedScripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkPolicy||])

currencySymbol :: CurrencySymbol
currencySymbol = ScriptUtils.scriptCurrencySymbol mintingPolicy
