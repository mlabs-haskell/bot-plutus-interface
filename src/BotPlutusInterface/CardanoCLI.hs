{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.CardanoCLI (
  submitTx,
  calculateMinUtxo,
  calculateMinFee,
  buildTx,
  signTx,
  validatorScriptFilePath,
  unsafeSerialiseAddress,
  policyScriptFilePath,
  utxosAt,
  queryTip,
) where

import BotPlutusInterface.Effects (PABEffect, ShellArgs (..), callCommand)
import BotPlutusInterface.Files (
  DummyPrivKey (FromSKey, FromVKey),
  datumJsonFilePath,
  metadataFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  signingKeyFilePath,
  txFilePath,
  validatorScriptFilePath,
 )
import BotPlutusInterface.Types (
  MintBudgets,
  PABConfig,
  SpendBudgets,
  Tip,
  TxBudget,
  mintBudgets,
  spendBudgets,
 )
import BotPlutusInterface.UtxoParser qualified as UtxoParser
import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Control.Monad (join)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft)
import Data.Hex (hex)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Ledger (Slot (Slot), SlotRange)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (..))
import Ledger.Crypto (PubKey, PubKeyHash (getPubKeyHash))
import Ledger.Interval (
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Ledger.Scripts (Datum, DatumHash (..))
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
  txId,
 )
import Ledger.TxId (TxId (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.Contract.CardanoAPI (toCardanoAddress)
import Plutus.V1.Ledger.Ada (fromValue, getLovelace)
import Plutus.V1.Ledger.Api (
  CurrencySymbol (..),
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  TokenName (..),
 )
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin)
import Prelude

-- | Getting information of the latest block
queryTip ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Eff effs (Either Text Tip)
queryTip config =
  callCommand @w
    ShellArgs
      { cmdName = "cardano-cli"
      , cmdArgs = mconcat [["query", "tip"], networkOpt config]
      , cmdOutParser = fromMaybe (error "Couldn't parse chain tip") . JSON.decode . Char8.pack
      }

-- | Getting all available UTXOs at an address (all utxos are assumed to be PublicKeyChainIndexTxOut)
utxosAt ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Address ->
  Eff effs (Either Text (Map TxOutRef ChainIndexTxOut))
utxosAt pabConf address =
  callCommand @w
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

calculateMinUtxo ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map DatumHash Datum ->
  TxOut ->
  Eff effs (Either Text Integer)
calculateMinUtxo pabConf datums txOut =
  join
    <$> callCommand @w
      ShellArgs
        { cmdName = "cardano-cli"
        , cmdArgs =
            mconcat
              [ ["transaction", "calculate-min-required-utxo", "--alonzo-era"]
              , txOutOpts pabConf datums [txOut]
              , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
              ]
        , cmdOutParser = mapLeft Text.pack . parseOnly UtxoParser.feeParser . Text.pack
        }

-- | Calculating fee for an unbalanced transaction
calculateMinFee ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Tx ->
  Eff effs (Either Text Integer)
calculateMinFee pabConf tx =
  join
    <$> callCommand @w
      ShellArgs
        { cmdName = "cardano-cli"
        , cmdArgs =
            mconcat
              [ ["transaction", "calculate-min-fee"]
              , ["--tx-body-file", txFilePath pabConf "raw" (txId tx)]
              , ["--tx-in-count", showText $ length $ txInputs tx]
              , ["--tx-out-count", showText $ length $ txOutputs tx]
              , ["--witness-count", showText $ length $ txSignatures tx]
              , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
              , networkOpt pabConf
              ]
        , cmdOutParser = mapLeft Text.pack . parseOnly UtxoParser.feeParser . Text.pack
        }

-- | Build a tx body and write it to disk
buildTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  TxBudget ->
  Tx ->
  Eff effs (Either Text ExBudget)
buildTx pabConf privKeys txBudget tx = do
  let (ins, valBudget) = txInOpts (spendBudgets txBudget) pabConf (txInputs tx)
      (mints, mintBudget) = mintOpts (mintBudgets txBudget) pabConf (txMintScripts tx) (txRedeemers tx) (txMint tx)
  callCommand @w $ ShellArgs "cardano-cli" (opts ins mints) (const $ valBudget <> mintBudget)
  where
    requiredSigners =
      concatMap
        ( \pubKey ->
            let pkh = Ledger.pubKeyHash pubKey
             in case Map.lookup pkh privKeys of
                  Just (FromSKey _) ->
                    ["--required-signer", signingKeyFilePath pabConf pkh]
                  Just (FromVKey _) ->
                    ["--required-signer-hash", encodeByteString $ fromBuiltin $ getPubKeyHash pkh]
                  Nothing ->
                    []
        )
        (Map.keys (Ledger.txSignatures tx))
    opts ins mints =
      mconcat
        [ ["transaction", "build-raw", "--alonzo-era"]
        , ins
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txData tx) (txOutputs tx)
        , mints
        , validRangeOpts (txValidRange tx)
        , metadataOpts pabConf (txMetadata tx)
        , requiredSigners
        , ["--fee", showText . getLovelace . fromValue $ txFee tx]
        , mconcat
            [ ["--protocol-params-file", pabConf.pcProtocolParamsFile]
            , ["--out-file", txFilePath pabConf "raw" (txId tx)]
            ]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
signTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Tx ->
  [PubKey] ->
  Eff effs (Either Text ())
signTx pabConf tx pubKeys =
  callCommand @w $ ShellArgs "cardano-cli" opts (const ())
  where
    signingKeyFiles =
      concatMap
        (\pubKey -> ["--signing-key-file", signingKeyFilePath pabConf (Ledger.pubKeyHash pubKey)])
        pubKeys

    opts =
      mconcat
        [ ["transaction", "sign"]
        , ["--tx-body-file", txFilePath pabConf "raw" (txId tx)]
        , signingKeyFiles
        , ["--out-file", txFilePath pabConf "signed" (txId tx)]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
submitTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Tx ->
  Eff effs (Either Text ())
submitTx pabConf tx =
  callCommand @w $
    ShellArgs
      "cardano-cli"
      ( mconcat
          [ ["transaction", "submit"]
          , ["--tx-file", txFilePath pabConf "signed" (txId tx)]
          , networkOpt pabConf
          ]
      )
      (const ())

txInOpts :: SpendBudgets -> PABConfig -> Set TxIn -> ([Text], ExBudget)
txInOpts spendIndex pabConf =
  foldMap
    ( \(TxIn txOutRef txInType) ->
        let (opts, exBudget) =
              scriptInputs
                txInType
                (Map.findWithDefault mempty txOutRef spendIndex)
         in (,exBudget) $
              mconcat
                [ ["--tx-in", txOutRefToCliArg txOutRef]
                , opts
                ]
    )
    . Set.toList
  where
    scriptInputs :: Maybe TxInType -> ExBudget -> ([Text], ExBudget)
    scriptInputs txInType exBudget =
      case txInType of
        Just (ConsumeScriptAddress validator redeemer datum) ->
          (,exBudget) $
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
              ,
                [ "--tx-in-execution-units"
                , exBudgetToCliArg exBudget
                ]
              ]
        Just ConsumePublicKeyAddress -> mempty
        Just ConsumeSimpleScriptAddress -> mempty
        Nothing -> mempty

txInCollateralOpts :: Set TxIn -> [Text]
txInCollateralOpts =
  concatMap (\(TxIn txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef]) . Set.toList

-- Minting options
mintOpts :: MintBudgets -> PABConfig -> Set Scripts.MintingPolicy -> Redeemers -> Value -> ([Text], ExBudget)
mintOpts mintIndex pabConf mintingPolicies redeemers mintValue =
  let scriptOpts =
        foldMap
          ( \(idx, policy) ->
              let redeemerPtr = RedeemerPtr Mint idx
                  redeemer = Map.lookup redeemerPtr redeemers
                  curSymbol = Value.mpsSymbol $ Scripts.mintingPolicyHash policy
                  exBudget =
                    Map.findWithDefault
                      mempty
                      (Scripts.mintingPolicyHash policy)
                      mintIndex
                  toOpts r =
                    (,exBudget) $
                      mconcat
                        [ ["--mint-script-file", policyScriptFilePath pabConf curSymbol]
                        , ["--mint-redeemer-file", redeemerJsonFilePath pabConf (Ledger.redeemerHash r)]
                        , ["--mint-execution-units", exBudgetToCliArg exBudget]
                        ]
               in orMempty $ fmap toOpts redeemer
          )
          $ zip [0 ..] $ Set.toList mintingPolicies
      mintOpt =
        if not (Value.isZero mintValue)
          then ["--mint", valueToCliArg mintValue]
          else []
   in first (<> mintOpt) scriptOpts

orMempty :: forall (m :: Type). Monoid m => Maybe m -> m
orMempty = fromMaybe mempty

-- | This function does not check if the range is valid, for that see `PreBalance.validateRange`
validRangeOpts :: SlotRange -> [Text]
validRangeOpts (Interval lowerBound upperBound) =
  mconcat
    [ case lowerBound of
        LowerBound (Finite (Slot x)) closed ->
          ["--invalid-before", showText (bool (x + 1) x closed)]
        _ -> []
    , case upperBound of
        UpperBound (Finite (Slot x)) closed ->
          ["--invalid-hereafter", showText (bool x (x + 1) closed)]
        _ -> []
    ]

txOutOpts :: PABConfig -> Map DatumHash Datum -> [TxOut] -> [Text]
txOutOpts pabConf datums =
  concatMap
    ( \TxOut {txOutAddress, txOutValue, txOutDatumHash} ->
        mconcat
          [
            [ "--tx-out"
            , Text.intercalate
                "+"
                [ unsafeSerialiseAddress pabConf.pcNetwork txOutAddress
                , valueToCliArg txOutValue
                ]
            ]
          , case txOutDatumHash of
              Nothing -> []
              Just datumHash@(DatumHash dh) ->
                if Map.member datumHash datums
                  then ["--tx-out-datum-embed-file", datumJsonFilePath pabConf datumHash]
                  else ["--tx-out-datum-hash", encodeByteString $ fromBuiltin dh]
          ]
    )

networkOpt :: PABConfig -> [Text]
networkOpt pabConf = case pabConf.pcNetwork of
  Testnet (NetworkMagic t) -> ["--testnet-magic", showText t]
  Mainnet -> ["--mainnet"]

txOutRefToCliArg :: TxOutRef -> Text
txOutRefToCliArg (TxOutRef (TxId tId) txIx) =
  encodeByteString (fromBuiltin tId) <> "#" <> showText txIx

flatValueToCliArg :: (CurrencySymbol, TokenName, Integer) -> Text
flatValueToCliArg (curSymbol, name, amount)
  | curSymbol == Ada.adaSymbol = amountStr
  | Text.null tokenNameStr = amountStr <> " " <> curSymbolStr
  | otherwise = amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
  where
    amountStr = showText amount
    curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
    tokenNameStr = decodeUtf8 $ hex $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

unsafeSerialiseAddress :: NetworkId -> Address -> Text
unsafeSerialiseAddress network address =
  case serialiseAddress <$> toCardanoAddress network address of
    Right a -> a
    Left _ -> error "Couldn't create address"

exBudgetToCliArg :: ExBudget -> Text
exBudgetToCliArg (ExBudget (ExCPU steps) (ExMemory memory)) =
  "(" <> showText steps <> "," <> showText memory <> ")"

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

-- -- TODO: There is some issue with this function, the generated wallet key is incorrect
-- toWalletKey :: Wallet -> Text
-- toWalletKey =
--   decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub

metadataOpts :: PABConfig -> Maybe BuiltinByteString -> [Text]
metadataOpts _ Nothing = mempty
metadataOpts pabConf (Just meta) =
  ["--metadata-json-file", metadataFilePath pabConf meta]
