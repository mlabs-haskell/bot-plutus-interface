{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module BotPlutusInterface.CardanoCLI (
  submitTx,
  calculateMinFee,
  buildTx,
  signTx,
  validatorScriptFilePath,
  policyScriptFilePath,
  queryTip,
) where

import BotPlutusInterface.Effects (PABEffect, ShellArgs (..), callCommand)
import BotPlutusInterface.Files (
  datumJsonFilePath,
  metadataFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  referenceScriptFilePath,
  signingKeyFilePath,
  txFilePath,
  validatorScriptFilePath,
 )
import BotPlutusInterface.Helpers (isZero)
import BotPlutusInterface.Types (
  EstimationContext (ecUtxos),
  MintBudgets,
  PABConfig (..),
  SpendBudgets,
  Tip,
  TxBudget,
  mintBudgets,
  spendBudgets,
 )
import BotPlutusInterface.UtxoParser qualified as UtxoParser
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  ReferenceScript (ReferenceScript),
  serialiseAddress,
 )
import Cardano.Prelude (note)
import Control.Monad (join)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State (State, get)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Either.Combinators (mapLeft)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (Slot (Slot), SlotRange, Tx (txCollateralInputs, txMintingWitnesses))
import Ledger qualified
import Ledger.Ada (fromValue, getLovelace)
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
  Tx (
    txData,
    txFee,
    txInputs,
    txMetadata,
    txMint,
    txOutputs,
    txReferenceInputs,
    txSignatures,
    txValidRange
  ),
  TxId (TxId),
  TxInput (TxInput),
  TxInputType (TxScriptAddress),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  txId,
 )
import Ledger.Tx.CardanoAPI (fromCardanoScriptInAnyLang, toCardanoValue)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
 )
import Plutus.V1.Ledger.Bytes qualified as Bytes
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
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
              , ["--protocol-params-file", (pcProtocolParamsFile pabConf)]
              , networkOpt pabConf
              ]
        , cmdOutParser = mapLeft Text.pack . parseOnly UtxoParser.feeParser . Text.pack
        }

{- | The transaction type we must work with (provided by plutus-apps) stores required signatures as a mapping of `Map PubKey Signature`
 However, in the case of partial signatures (keys to be added to required signers as Pubkeyhash, but not signed by BPI),
 we do not know, nor do we need, the full Pubkey. Unfortunately, this forces us to put a Pubkeyhash into the Pubkey data type,
 to get it past the plutus-apps -> BPI layer (via the Tx type).
 This function takes a Pubkey that we know may have this intentionally malformed encoding and:
   If the pubkey inside is a real pubkey, hash it using the Ledger function
   if the "pubkey" inside is actually a pubkeyhash, return this directly
 We test for this using the length. A pubkeyhash is 28 bytes, whereas a pubkey is longer.
-}
pubKeyToPubKeyHashHack :: Ledger.PubKey -> Ledger.PubKeyHash
pubKeyToPubKeyHashHack pk@(Ledger.PubKey ledgerBytes) =
  let bytes = Bytes.bytes ledgerBytes
   in if BS.length bytes > 28
        then Ledger.pubKeyHash pk
        else Ledger.PubKeyHash $ toBuiltin bytes

-- | Build a tx body and write it to disk
buildTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  ( Member (PABEffect w) effs
  , Member (State EstimationContext) effs
  ) =>
  PABConfig ->
  TxBudget ->
  Tx ->
  Eff effs (Either Text ())
buildTx pabConf txBudget tx = do
  utxos <- ecUtxos <$> get
  let requiredSigners =
        concatMap
          (\pubKey -> ["--required-signer-hash", encodeByteString $ fromBuiltin $ getPubKeyHash $ pubKeyToPubKeyHashHack pubKey])
          (Map.keys (Ledger.txSignatures tx))
      opts ins mints =
        mconcat
          [ ["transaction", "build-raw", "--babbage-era"]
          , ins
          , txRefInputOpts (txReferenceInputs tx)
          , txInputCollateralOpts (txCollateralInputs tx)
          , txOutOpts pabConf (txData tx) (txOutputs tx)
          , mints
          , validRangeOpts (txValidRange tx)
          , metadataOpts pabConf (txMetadata tx)
          , requiredSigners
          , ["--fee", showText . getLovelace . fromValue $ txFee tx]
          , mconcat
              [ ["--protocol-params-file", (pcProtocolParamsFile pabConf)]
              , ["--out-file", txFilePath pabConf "raw" (txId tx)]
              ]
          ]
  case toCardanoValue $ txMint tx of
    Right mintValue -> do
      let eIns = txInputOpts (spendBudgets txBudget) pabConf utxos (txInputs tx)
          mints = mintOpts (mintBudgets txBudget) pabConf (fst <$> txMintingWitnesses tx) mintValue
      either (pure . Left) (\ins -> callCommand @w $ ShellArgs "cardano-cli" (opts ins mints) (const ())) eIns
    Left err -> pure $ Left $ showText err

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

txInputOpts :: SpendBudgets -> PABConfig -> Map TxOutRef (CApi.TxOut CApi.CtxUTxO CApi.BabbageEra) -> [TxInput] -> Either Text [Text]
txInputOpts spendIndex pabConf utxos =
  fmap mconcat
    . traverse
      ( \(TxInput txOutRef txInputType) -> do
          scriptInputs <- mkScriptInputs txInputType txOutRef (Map.findWithDefault mempty txOutRef spendIndex)
          pure $
            mconcat
              [ ["--tx-in", txOutRefToCliArg txOutRef]
              , scriptInputs
              ]
      )
  where
    mkScriptInputs :: TxInputType -> TxOutRef -> ExBudget -> Either Text [Text]
    mkScriptInputs txInputType txOutRef exBudget =
      case txInputType of
        TxScriptAddress redeemer eVHash dHash -> do
          let (typeText, prefix) = getTxInTypeAndPrefix eVHash
          datumOpts <- handleTxInDatum prefix txOutRef dHash
          pure $
            mconcat
              [ typeText
              , datumOpts
              ,
                [ prefix <> "tx-in-redeemer-file"
                , redeemerJsonFilePath pabConf (ScriptUtils.redeemerHash redeemer)
                ]
              ,
                [ prefix <> "tx-in-execution-units"
                , exBudgetToCliArg exBudget
                ]
              ]
        _ -> pure []

    getTxInTypeAndPrefix :: Either Scripts.ValidatorHash (ScriptUtils.Versioned TxOutRef) -> ([Text], Text)
    getTxInTypeAndPrefix = \case
      Left vHash ->
        (
          [ "--tx-in-script-file"
          , validatorScriptFilePath pabConf vHash
          ]
        , "--"
        )
      Right versionedTxOutRef ->
        ( [ "--spending-tx-in-reference"
          , txOutRefToCliArg $ ScriptUtils.unversioned versionedTxOutRef
          ]
            ++ case ScriptUtils.version versionedTxOutRef of
              ScriptUtils.PlutusV1 -> []
              ScriptUtils.PlutusV2 -> ["--spending-plutus-script-v2"]
        , "--spending-reference-"
        )

    txOutDatumIsInline :: CApi.TxOut CApi.CtxUTxO CApi.BabbageEra -> Bool
    txOutDatumIsInline (CApi.TxOut _ _ (CApi.TxOutDatumInline _ _) _) = True
    txOutDatumIsInline _ = False

    handleTxInDatum :: Text -> TxOutRef -> Maybe DatumHash -> Either Text [Text]
    handleTxInDatum prefix txOutRef mDHash =
      if maybe False txOutDatumIsInline (Map.lookup txOutRef utxos)
        then pure [prefix <> "tx-in-inline-datum-present"]
        else do
          dHash <- note "CLI Cannot handle TxOutDatumNone" mDHash
          pure
            [ prefix <> "tx-in-datum-file"
            , datumJsonFilePath pabConf dHash
            ]

txRefInputOpts :: [TxInput] -> [Text]
txRefInputOpts =
  foldMap $
    \(TxInput txOutRef _) ->
      ["--read-only-tx-in-reference", txOutRefToCliArg txOutRef]

txInputCollateralOpts :: [TxInput] -> [Text]
txInputCollateralOpts =
  concatMap (\(TxInput txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef])

-- Minting options
mintOpts ::
  MintBudgets ->
  PABConfig ->
  Map Scripts.MintingPolicyHash Ledger.Redeemer ->
  CApi.Value ->
  [Text]
mintOpts mintIndex pabConf redeemers mintValue =
  let scriptOpts =
        Map.foldMapWithKey
          ( \mph redeemer ->
              let curSymbol = Value.mpsSymbol mph
                  exBudget =
                    Map.findWithDefault
                      mempty
                      mph
                      mintIndex
               in mconcat
                    [ ["--mint-script-file", policyScriptFilePath pabConf curSymbol]
                    , ["--mint-redeemer-file", redeemerJsonFilePath pabConf (ScriptUtils.redeemerHash redeemer)]
                    , ["--mint-execution-units", exBudgetToCliArg exBudget]
                    ]
          )
          redeemers
      mintOpt =
        if not (isZero mintValue)
          then ["--mint", valueToCliArg mintValue]
          else []
   in scriptOpts <> mintOpt

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
    ( \(TxOut (CApi.TxOut addr val datum refScript)) ->
        mconcat
          [
            [ "--tx-out"
            , Text.intercalate
                "+"
                [ serialiseAddress addr
                , valueToCliArg $ CApi.txOutValueToValue val
                ]
            ]
          , case datum of
              CApi.TxOutDatumNone -> []
              CApi.TxOutDatumInTx _ scriptData -> datumTextFromScriptDataHash $ CApi.hashScriptData scriptData
              CApi.TxOutDatumHash _ scriptDataHash -> datumTextFromScriptDataHash scriptDataHash
              CApi.TxOutDatumInline _ scriptData ->
                let datumHash = DatumHash $ toBuiltin $ CApi.serialiseToRawBytes $ CApi.hashScriptData scriptData
                 in ["--tx-out-inline-datum-file", datumJsonFilePath pabConf datumHash]
          , case refScript of
              ReferenceScript _ (fromCardanoScriptInAnyLang -> Just vScript) ->
                ["--tx-out-reference-script-file", referenceScriptFilePath pabConf $ ScriptUtils.scriptHash vScript]
              _ -> []
          ]
    )
  where
    datumTextFromScriptDataHash :: CApi.Hash CApi.ScriptData -> [Text]
    datumTextFromScriptDataHash scriptDataHash =
      let dh = CApi.serialiseToRawBytes scriptDataHash
          datumHash = DatumHash $ toBuiltin dh
       in if Map.member datumHash datums
            then ["--tx-out-datum-embed-file", datumJsonFilePath pabConf datumHash]
            else ["--tx-out-datum-hash", encodeByteString dh]

networkOpt :: PABConfig -> [Text]
networkOpt PABConfig {pcNetwork} = case pcNetwork of
  Testnet (NetworkMagic t) -> ["--testnet-magic", showText t]
  Mainnet -> ["--mainnet"]

txOutRefToCliArg :: TxOutRef -> Text
txOutRefToCliArg (TxOutRef (TxId tId) txIx) =
  encodeByteString (fromBuiltin tId) <> "#" <> showText txIx

flatValueToCliArg :: (CApi.AssetId, CApi.Quantity) -> Text
flatValueToCliArg (CApi.AdaAssetId, qty) = showText qty
flatValueToCliArg (CApi.AssetId policyId assetName, qty)
  | assetName == "" = showText qty <> " " <> serialise policyId
  | otherwise = showText qty <> " " <> serialise policyId <> "." <> serialise assetName
  where
    serialise :: forall (a :: Type). CApi.SerialiseAsRawBytes a => a -> Text
    serialise = Text.toLower . CApi.serialiseToRawBytesHexText

valueToCliArg :: CApi.Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ CApi.valueToList val

exBudgetToCliArg :: ExBudget -> Text
exBudgetToCliArg (ExBudget (ExCPU steps) (ExMemory memory)) =
  "(" <> showText steps <> "," <> showText memory <> ")"

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

metadataOpts :: PABConfig -> Maybe BuiltinByteString -> [Text]
metadataOpts _ Nothing = mempty
metadataOpts pabConf (Just meta) =
  ["--metadata-json-file", metadataFilePath pabConf meta]
