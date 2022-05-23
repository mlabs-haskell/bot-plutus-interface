{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.PlutusExample.NFT where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad (void)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger (
  CurrencySymbol,
  PaymentPubKeyHash,
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName,
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs, txInfoMint),
  TxOutRef,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptCurrencySymbol,
 )
import Ledger.Address (StakePubKeyHash)
import Ledger.Constraints as Constraints
import Ledger.Constraints.Metadata (
  NftMetadata (NftMetadata),
  NftMetadataToken (NftMetadataToken),
  TxMetadata (TxMetadata),
  nmtDescription,
  nmtFiles,
  nmtImage,
  nmtMediaType,
  nmtName,
  nmtOtherFields,
 )
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value (flattenValue, singleton)
import Plutus.Contract (Contract, Endpoint, submitTxConstraintsWith, tell, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude
import Text.Printf (printf)
import Prelude qualified as Hask

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

data MintParams = MintParams
  { mpName :: Text
  , mpDescription :: Maybe Text
  , mpImage :: Text
  , mpTokenName :: TokenName
  , mpPubKeyHash :: PaymentPubKeyHash
  , mpStakeHash :: StakePubKeyHash
  }
  deriving stock (Hask.Show)

$(deriveJSON defaultOptions ''MintParams)

mintNft :: MintParams -> Contract (Last Text) NFTSchema Text ()
mintNft MintParams {..} = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  case Map.keys utxos of
    [] -> Contract.logError @Hask.String "no utxo found"
    oref : _ -> do
      tell $ Last $ Just $ "Using oref:" Hask.<> Text.pack (Hask.show oref)
      let cs = curSymbol oref mpTokenName
          val = singleton cs mpTokenName 1
          meta =
            NftMetadata $
              Map.singleton cs $
                Map.singleton mpTokenName $
                  NftMetadataToken
                    { nmtName = mpName
                    , nmtImage = mpImage
                    , nmtMediaType = Hask.pure "image/png"
                    , nmtDescription = mpDescription
                    , nmtFiles = Hask.mempty
                    , nmtOtherFields = Hask.mempty
                    }
          lookups =
            Hask.mconcat
              [ Constraints.mintingPolicy (policy oref mpTokenName)
              , Constraints.unspentOutputs utxos
              ]
          tx =
            Hask.mconcat
              [ Constraints.mustMintValue val
              , Constraints.mustSpendPubKeyOutput oref
              , Constraints.mustPayToPubKeyAddress mpPubKeyHash mpStakeHash val
              , Constraints.mustIncludeMetadata $ TxMetadata (Just meta) Hask.mempty
              ]
      void $ submitTxConstraintsWith @Void lookups tx
      Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)
      tell $ Last $ Just "Finished"
