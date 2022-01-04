{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.PlutusExample.Game where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad hiding (fmap)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
  validatorAddress,
  validatorHash,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, submitTxConstraints, submitTxConstraintsWith, utxosAt, type (.\/))
import Plutus.V1.Ledger.Scripts (Datum (Datum), Redeemer (Redeemer))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)), unless)
import Schema (
  ToSchema,
 )
import Prelude (Semigroup ((<>)), Show, String)

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> BuiltinByteString -> BuiltinByteString -> Ledger.ScriptContext -> Bool
mkValidator _ datum redeemer _ =
  traceIfFalse "Bad guess" $ datum == sha2_256 redeemer

gameValidator :: Integer -> TypedValidator Game
gameValidator gameId =
  Scripts.mkTypedValidator @Game
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode gameId)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BuiltinByteString @BuiltinByteString

data Game
instance ValidatorTypes Game where
  type RedeemerType Game = BuiltinByteString
  type DatumType Game = BuiltinByteString

script :: Integer -> Ledger.Script
script = Scripts.unValidatorScript . Scripts.validatorScript . gameValidator

lockScriptSBS :: Integer -> SBS.ShortByteString
lockScriptSBS gameId = SBS.toShort . LBS.toStrict $ serialise $ script gameId

lockScript :: Integer -> PlutusScript PlutusScriptV1
lockScript gameId = PlutusScriptSerialised $ lockScriptSBS gameId

type GameSchema =
  Endpoint "lock" LockParams .\/ Endpoint "guess" GuessParams

data LockParams = LockParams
  { lockGameId :: Integer
  , lockAmount :: Integer
  , lockSecret :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data GuessParams = GuessParams
  { guessGameId :: Integer
  , guessSecret :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''LockParams)
$(deriveJSON defaultOptions ''GuessParams)

lock :: LockParams -> Contract () GameSchema Text ()
lock LockParams {lockGameId = gameId, lockAmount = amount, lockSecret = secret} = do
  let val = Ada.lovelaceValueOf amount
      validator = gameValidator gameId
      valHash = validatorHash validator
      datum = Datum $ PlutusTx.toBuiltinData $ sha2_256 $ toBuiltin $ ByteString.pack secret
      tx = Constraints.mustPayToOtherScript valHash datum val
  void $ submitTxConstraints @Game validator tx

guess :: GuessParams -> Contract () GameSchema Text ()
guess GuessParams {guessGameId = gameId, guessSecret = secret} = do
  let validator = gameValidator gameId
      valAddr = validatorAddress validator
      redeemer = Redeemer $ PlutusTx.toBuiltinData $ toBuiltin $ ByteString.pack secret

  utxos <- utxosAt valAddr
  let lookups =
        Constraints.otherScript (Scripts.validatorScript validator)
          <> Constraints.unspentOutputs utxos
      tx = mconcat $ map (`Constraints.mustSpendScriptOutput` redeemer) $ Map.keys utxos

  void $ submitTxConstraintsWith @Game lookups tx
