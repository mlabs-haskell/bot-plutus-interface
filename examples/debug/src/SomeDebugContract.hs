module SomeDebugContract where

import Data.Text (Text)

import Data.Map (size)
import Data.Map qualified as M
import Debug.Trace (traceM)
import Ledger (Address (Address), PaymentPubKeyHash (PaymentPubKeyHash), getCardanoTxId)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, submitTx)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (adaValueOf)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import PlutusTx.Prelude
import Text.Show.Pretty (ppShow)
import Tools
import Prelude qualified as Hask

utxosAtDebug :: Contract () EmptySchema Text ()
utxosAtDebug = do
  let pkh = pkhFromHash "f433ae2392c0491a9b49acd9ca94033dafba13f8bcd3df5aa840b738"
      addr = Address (PubKeyCredential pkh) Nothing
  utxos <- Contract.utxosAt addr
  traceM $ "UTXOs len: " ++ Hask.show (length $ M.toList utxos)

payToHardcodedPKH :: Contract () EmptySchema Text ()
payToHardcodedPKH = do
  res <- Contract.runError contract
  traceM $ "Contract res: " <> Hask.show res
  pure ()
  where
    contract :: Contract () EmptySchema Text ()
    contract = do
      ownPPkh <- Contract.ownPaymentPubKeyHash
      let (PaymentPubKeyHash ownPkh) = ownPPkh
      let payToPkh = PaymentPubKeyHash $ pkhFromHash "f433ae2392c0491a9b49acd9ca94033dafba13f8bcd3df5aa840b738"
          ownAddr = Address (PubKeyCredential ownPkh) Nothing

      let txc =
            Constraints.mustPayToPubKey payToPkh (adaValueOf 44)
              <> Constraints.mustPayToPubKey ownPPkh (adaValueOf 21)
              <> Constraints.mustPayToPubKey ownPPkh (adaValueOf 33)

      utxosBefore <- Contract.utxosAt ownAddr
      traceM $ "UTxOs BEFORE: " <> ppShow utxosBefore

      tx <- submitTx txc
      Contract.awaitTxConfirmed (getCardanoTxId tx)

      utxosAfter <- Contract.utxosAt ownAddr
      traceM $ "UTxOs Size AFTER: " <> Hask.show (size utxosAfter)
      traceM $ "UTxOs AFTER: " <> ppShow utxosAfter
