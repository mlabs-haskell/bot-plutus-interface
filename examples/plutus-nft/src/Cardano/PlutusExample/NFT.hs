{-# LANGUAGE TemplateHaskell #-}

module Cardano.PlutusExample.NFT where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad hiding (fmap)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value as Value
import Plutus.Contract (Contract, Endpoint, submitTxConstraintsWith, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (Semigroup (..), String, show)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> BuiltinData -> ScriptContext -> Bool
mkPolicy oref tn _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
      _ -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' tn' -> wrapMintingPolicy $ mkPolicy oref' tn'||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tn

policyScript :: TxOutRef -> TokenName -> Script
policyScript oref tn = Scripts.unMintingPolicyScript $ policy oref tn

policySBS :: TxOutRef -> TokenName -> SBS.ShortByteString
policySBS oref tn = SBS.toShort . LBS.toStrict $ serialise $ policyScript oref tn

policySerialised :: TxOutRef -> TokenName -> PlutusScript PlutusScriptV1
policySerialised oref tn = PlutusScriptSerialised $ policySBS oref tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

type NFTSchema =
  Endpoint "mint" TokenName

mintNft :: TokenName -> Contract Text NFTSchema Text ()
mintNft tn = do
  pkh <- Contract.ownPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh)
  case Map.keys utxos of
    [] -> Contract.logError @String "no utxo found"
    oref : _ -> do
      let val = Value.singleton (curSymbol oref tn) tn 1
          lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
          tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
      void $ submitTxConstraintsWith @Void lookups tx
      Contract.logInfo @String $ printf "forged %s" (show val)
