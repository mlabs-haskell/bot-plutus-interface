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
  DummyPrivKey (FromSKey, FromVKey),
  datumJsonFilePath,
  -- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
  -- metadataFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  referenceScriptFilePath,
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
import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  ReferenceScript (ReferenceScript),
  serialiseAddress,
 )
import Control.Monad (join)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Either.Combinators (mapLeft)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (Slot (Slot), SlotRange)
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
    txCollateral,
    txData,
    txFee,
    txInputs,
    txMint,
    txMintingScripts,
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
import Ledger.Tx.CardanoAPI (toCardanoValue, fromCardanoScriptInAnyLang)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
 )
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
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
buildTx pabConf privKeys txBudget tx =
  case toCardanoValue $ txMint tx of
    Right mintValue -> do
      let (ins, valBudget) = txInputOpts (spendBudgets txBudget) pabConf (txInputs tx)
          (mints, mintBudget) = mintOpts (mintBudgets txBudget) pabConf (txMintingScripts tx) mintValue
      callCommand @w $ ShellArgs "cardano-cli" (opts ins mints) (const $ valBudget <> mintBudget)
    Left err -> pure $ Left $ showText err
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
        [ ["transaction", "build-raw", "--babbage-era"]
        , ins
        , txRefInputOpts (txReferenceInputs tx)
        , txInputCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txData tx) (txOutputs tx)
        , mints
        , validRangeOpts (txValidRange tx)
        , -- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
          -- , metadataOpts pabConf (txMetadata tx)
          requiredSigners
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

txInputOpts :: SpendBudgets -> PABConfig -> [TxInput] -> ([Text], ExBudget)
txInputOpts spendIndex pabConf =
  foldMap $
    \(TxInput txOutRef txInputType) ->
      let (opts, exBudget) =
            scriptInputs
              txInputType
              (Map.findWithDefault mempty txOutRef spendIndex)
       in (,exBudget) $
            mconcat
              [ ["--tx-in", txOutRefToCliArg txOutRef]
              , opts
              ]
  where
    scriptInputs :: TxInputType -> ExBudget -> ([Text], ExBudget)
    scriptInputs txInputType exBudget =
      case txInputType of
        TxScriptAddress redeemer eVHash dHash ->
          let (typeText, prefix) = getTxInTypeAndPrefix eVHash
           in (,exBudget) $
                mconcat
                  [ typeText
                  ,
                    [ prefix <> "tx-in-datum-file"
                    , datumJsonFilePath pabConf dHash
                    ]
                  ,
                    [ prefix <> "tx-in-redeemer-file"
                    , redeemerJsonFilePath pabConf (ScriptUtils.redeemerHash redeemer)
                    ]
                  ,
                    [ prefix <> "tx-in-execution-units"
                    , exBudgetToCliArg exBudget
                    ]
                  ]
        _ -> mempty
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

txRefInputOpts :: [TxInput] -> [Text]
txRefInputOpts =
  foldMap $
    \(TxInput txOutRef _) ->
      ["--read-only-tx-in-reference", txOutRefToCliArg txOutRef]

txInputCollateralOpts :: [TxInput] -> [Text]
txInputCollateralOpts =
  concatMap (\(TxInput txOutRef _) -> ["--tx-in-collateral", txOutRefToCliArg txOutRef])

isZero :: CApi.Value -> Bool
isZero = all ((== 0) . snd) . CApi.valueToList

-- Minting options
mintOpts ::
  MintBudgets ->
  PABConfig ->
  Map Scripts.MintingPolicyHash Ledger.Redeemer ->
  CApi.Value ->
  ([Text], ExBudget)
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
               in (,exBudget) $
                    mconcat
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
   in first (<> mintOpt) scriptOpts

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
    ( \(TxOut (CApi.TxOut (CApi.AddressInEra _ addr) val datum refScript)) ->
        mconcat
          [
            [ "--tx-out"
            , Text.intercalate
                "+"
                [ serialiseAddress $ CApi.toAddressAny addr
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
              -- TODO: Writing a reference script.
              -- Second arg is ScriptInAnyLang, takes path to the script.
              -- Need to update Files.hs to write scripts from here.
              -- no way to know if minting/validator without reading the UPLC, so lets give these a new naming scheme
              -- This script can be simplev1/v2 or plutusv1/v2 (we'll not handle simple)
              -- As such, helper function in Files will give name by script hash, prefixed with `reference` to avoid clash (though a clash is actually fine)
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
networkOpt pabConf = case pabConf.pcNetwork of
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

-- TODO: Removed for now, as the main iohk branch doesn't support metadata yet
-- metadataOpts :: PABConfig -> Maybe BuiltinByteString -> [Text]
-- metadataOpts _ Nothing = mempty
-- metadataOpts pabConf (Just meta) =
--   ["--metadata-json-file", metadataFilePath pabConf meta]
