{-# LANGUAGE NamedFieldPuns #-}

module MLabsPAB.CardanoCLI (
  submitTx,
  buildTx,
  signTx,
  uploadFiles,
  validatorScriptFilePath,
  unsafeSerialiseAddress,
  policyScriptFilePath,
  utxosAt,
) where

import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Crypto (PubKey)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut,
  RedeemerPtr (..),
  Redeemers,
  ScriptTag (..),
  Tx (..),
  TxIn (..),
  TxInType (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.TxId (TxId (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import MLabsPAB.Effects (PABEffect, ShellArgs (..), callCommand, uploadDir)
import MLabsPAB.Files (
  datumJsonFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  signingKeyFilePath,
  validatorScriptFilePath,
 )
import MLabsPAB.Types (PABConfig)
import MLabsPAB.UtxoParser qualified as UtxoParser
import Plutus.Contract.CardanoAPI (toCardanoAddress)
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (fromBuiltin)
import Prelude

-- | Upload script files to remote server
uploadFiles ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  Eff effs ()
uploadFiles pabConf =
  mapM_
    uploadDir
    [ pabConf.pcScriptFileDir
    , pabConf.pcSigningKeyFileDir
    ]

-- | Getting all available UTXOs at an address (all utxos are assumed to be PublicKeyChainIndexTxOut)
utxosAt ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  Address ->
  Eff effs (Map TxOutRef ChainIndexTxOut)
utxosAt pabConf address = do
  callCommand
    ShellArgs
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["query", "utxo"]
            , ["--address", unsafeSerialiseAddress pabConf.pcNetwork address]
            , networkOpt pabConf
            ]
      , cmdOutParser =
          Map.fromList
            . fromRight []
            . parseOnly (UtxoParser.utxoMapParser address)
            . Text.pack
      }

-- | Build a tx body and write it to disk
buildTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  PubKey ->
  Tx ->
  Eff effs ()
buildTx pabConf ownPubKey tx =
  callCommand $ ShellArgs "cardano-cli" opts (const ())
  where
    ownAddr = Ledger.pubKeyHashAddress $ Ledger.pubKeyHash ownPubKey
    requiredSigners =
      concatMap
        (\pubKey -> ["--required-signer", signingKeyFilePath pabConf (Ledger.pubKeyHash pubKey)])
        (Map.keys (Ledger.txSignatures tx))
    opts =
      mconcat
        [ ["transaction", "build", "--alonzo-era"]
        , txInOpts pabConf (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txOutputs tx)
        , mintOpts pabConf (txMintScripts tx) (txRedeemers tx) (txMint tx)
        , mconcat
            [ ["--change-address", unsafeSerialiseAddress pabConf.pcNetwork ownAddr]
            , requiredSigners
            , networkOpt pabConf
            , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
            , ["--out-file", "tx.raw"]
            ]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
signTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  [PubKey] ->
  Eff effs ()
signTx pabConf pubKeys =
  callCommand $
    ShellArgs
      "cardano-cli"
      ( mconcat
          [ ["transaction", "sign"]
          , ["--tx-body-file", "tx.raw"]
          , signingKeyFiles
          , ["--out-file", "tx.signed"]
          ]
      )
      (const ())
  where
    signingKeyFiles =
      concatMap
        (\pubKey -> ["--signing-key-file", signingKeyFilePath pabConf (Ledger.pubKeyHash pubKey)])
        pubKeys

-- Signs and writes a tx (uses the tx body written to disk as input)
submitTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  Eff effs (Maybe Text)
submitTx pabConf =
  callCommand $
    ShellArgs
      "cardano-cli"
      ( mconcat
          [ ["transaction", "submit"]
          , ["--tx-file", "tx.signed"]
          , networkOpt pabConf
          ]
      )
      ( ( \out ->
            if "Transaction successfully submitted." `Text.isPrefixOf` out
              then Nothing
              else Just out
        )
          . Text.pack
      )

txInOpts :: PABConfig -> Set TxIn -> [Text]
txInOpts pabConf =
  concatMap
    ( \(TxIn txOutRef txInType) ->
        mconcat
          [ ["--tx-in", txOutRefToCliArg txOutRef]
          , case txInType of
              Just (ConsumeScriptAddress validator redeemer datum) ->
                mconcat
                  [
                    [ "--tx-in-script-file"
                    , validatorScriptFilePath pabConf (Ledger.validatorHash validator)
                    ]
                  ,
                    [ "--tx-in-datum-file"
                    , datumJsonFilePath pabConf (Ledger.datumHash datum)
                    ]
                  ,
                    [ "--tx-in-redeemer-file"
                    , redeemerJsonFilePath pabConf (Ledger.redeemerHash redeemer)
                    ]
                  ]
              Just ConsumePublicKeyAddress -> []
              Just ConsumeSimpleScriptAddress -> []
              Nothing -> []
          ]
    )
    . Set.toList

txInCollateralOpts :: Set TxIn -> [Text]
txInCollateralOpts =
  concatMap (\(TxIn txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef]) . Set.toList

-- Minting options
mintOpts :: PABConfig -> Set Scripts.MintingPolicy -> Redeemers -> Value -> [Text]
mintOpts pabConf mintingPolicies redeemers mintValue =
  mconcat
    [ mconcat $
        concatMap
          ( \(idx, policy) ->
              let redeemerPtr = RedeemerPtr Mint idx
                  redeemer = Map.lookup redeemerPtr redeemers
                  curSymbol = Value.mpsSymbol $ Scripts.mintingPolicyHash policy
                  toOpts r =
                    [ ["--mint-script-file", policyScriptFilePath pabConf curSymbol]
                    , ["--mint-redeemer-file", redeemerJsonFilePath pabConf (Ledger.redeemerHash r)]
                    ]
               in mconcat $ maybeToList $ fmap toOpts redeemer
          )
          $ zip [0 ..] $ Set.toList mintingPolicies
    , if not (Value.isZero mintValue)
        then
          [ "--mint"
          , valueToCliArg mintValue
          ]
        else []
    ]

txOutOpts :: PABConfig -> [TxOut] -> [Text]
txOutOpts pabConf =
  concatMap
    ( \TxOut {txOutAddress, txOutValue} ->
        [ "--tx-out"
        , Text.intercalate
            "+"
            [ unsafeSerialiseAddress pabConf.pcNetwork txOutAddress
            , valueToCliArg txOutValue
            ]
        ]
    )

networkOpt :: PABConfig -> [Text]
networkOpt pabConf = case pabConf.pcNetwork of
  Testnet (NetworkMagic t) -> ["--testnet-magic", showText t]
  Mainnet -> ["--mainnet"]

txOutRefToCliArg :: TxOutRef -> Text
txOutRefToCliArg (TxOutRef (TxId txId) txIx) =
  encodeByteString (fromBuiltin txId) <> "#" <> showText txIx

flatValueToCliArg :: (CurrencySymbol, TokenName, Integer) -> Text
flatValueToCliArg (curSymbol, name, amount)
  | curSymbol == Ada.adaSymbol = amountStr
  | Text.null tokenNameStr = amountStr <> " " <> curSymbolStr
  | otherwise = amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
  where
    amountStr = showText amount
    curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
    tokenNameStr = decodeUtf8 $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

unsafeSerialiseAddress :: NetworkId -> Address -> Text
unsafeSerialiseAddress network address =
  case serialiseAddress <$> toCardanoAddress network address of
    Right a -> a
    Left _ -> error "Couldn't create address"

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

-- -- TODO: There is some issue with this function, the generated wallet key is incorrect
-- toWalletKey :: Wallet -> Text
-- toWalletKey =
--   decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub
