{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -w #-}

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

import BotPlutusInterface.Effects (PABEffect, ShellArgs (..), callCommand, queryChainIndex)
import BotPlutusInterface.Files (
  DummyPrivKey (FromSKey, FromVKey),
  datumJsonFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  signingKeyFilePath,
  txFilePath,
  validatorScriptFilePath,
 )
import BotPlutusInterface.Types (PABConfig (pcSlotConfig), Tip)
import BotPlutusInterface.UtxoParser qualified as UtxoParser
import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Codec.Serialise qualified as Codec
import Control.Monad (join)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.ByteString.Short qualified as ShortByteString
import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Hex (hex)
import Data.Kind (Type)
import Data.List (nub, sort, sortOn)
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
import Ledger.TimeSlot (slotRangeToPOSIXTimeRange)
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
import Plutus.ChainIndex.Tx (txOutRefMap)
import Plutus.Contract.CardanoAPI (toCardanoAddress)
import Plutus.Contract.Effects (ChainIndexQuery (TxsFromTxIds), ChainIndexResponse (TxIdsResponse))
import Plutus.V1.Ledger.Ada (fromValue, getLovelace)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol (..),
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  Script,
  ScriptContext (ScriptContext),
  TokenName (..),
  TxInInfo (TxInInfo),
  TxInfo (TxInfo),
 )
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (fromBuiltin)
import PlutusTx.Prelude qualified as PPrelude
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
              , ["--tx-body-file", txFilePath pabConf "raw" tx]
              , ["--tx-in-count", showText $ length $ txInputs tx]
              , ["--tx-out-count", showText $ length $ txOutputs tx]
              , ["--witness-count", showText $ length $ txSignatures tx]
              , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
              , networkOpt pabConf
              ]
        , cmdOutParser = mapLeft Text.pack . parseOnly UtxoParser.feeParser . Text.pack
        }

queryTxOuts ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  [TxId] ->
  Eff effs (Either Text (Map TxOutRef TxOut))
queryTxOuts txIds = do
  res <- queryChainIndex @w $ TxsFromTxIds txIds
  return $ case res of
    TxIdsResponse chainTxs -> Right $ foldMap (fmap (sortTxOut . fst) . txOutRefMap) chainTxs
    _ -> Left "Wrong PAB response"
  where
    sortTxOut :: TxOut -> TxOut
    sortTxOut txOut = txOut {txOutValue = sortValue $ txOutValue txOut}

-- There is no match txOutRefs request, and we don't want a separate PAB query per input.
-- So, for efficiency, we're going to query the transactions for all inputs combined,
-- then pick out the outputs we care about
getTxInInfos ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  [TxOutRef] ->
  Eff effs (Either Text [TxInInfo])
getTxInInfos txOutRefs = do
  let ids = nub $ txOutRefId <$> txOutRefs
  eAllOutRefs <- queryTxOuts @w ids
  return $
    eAllOutRefs >>= \allOutRefs ->
      sequence $ (\ref -> toEither $ TxInInfo ref <$> Map.lookup ref allOutRefs) <$> txOutRefs
  where
    toEither :: Maybe TxInInfo -> Either Text TxInInfo
    toEither = maybeToRight "Couldn't find TxOut"

buildTxInfo ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Tx ->
  Eff effs (Either Text TxInfo)
buildTxInfo pabConf tx = do
  let txOutRefs = txInRef <$> Set.toList (txInputs tx) -- This will already be in order, for Sets listify acsending
  eTxInInfos <- getTxInInfos @w txOutRefs
  return $
    (`second` eTxInInfos) $ \txInInfos ->
      TxInfo
        { txInfoInputs = txInInfos
        , txInfoOutputs = txOutputs tx
        , txInfoFee = sortValue $ txFee tx
        , txInfoMint = sortValue $ txMint tx
        , txInfoDCert = [] -- We don't support staking or stake redeeming at this time
        , txInfoWdrl = []
        , txInfoValidRange = slotRangeToPOSIXTimeRange (pcSlotConfig pabConf) $ txValidRange tx
        , txInfoSignatories = sort $ Ledger.pubKeyHash <$> Map.keys (txSignatures tx)
        , txInfoData = sortOn fst $ Map.toList $ txData tx
        , txInfoId = Ledger.txId tx
        }

-- | Sorts the internal maps
sortValue :: Value -> Value
sortValue (Value.Value m) = Value.Value $ sortMap $ sortMap PPrelude.<$> m
  where
    sortMap :: forall (k :: Type) (v :: Type). Ord k => AMap.Map k v -> AMap.Map k v
    sortMap = AMap.fromList . sortOn fst . AMap.toList

-- | Build a tx body and write it to disk
buildTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  Tx ->
  Eff effs (Either Text ExBudget)
buildTx pabConf privKeys tx = do
  eTxInfo <- buildTxInfo @w pabConf tx
  case eTxInfo of
    Right txInfo -> do
      let (ins, valBudget) = txInOpts pabConf txInfo (txInputs tx)
          (mints, mintBudget) = mintOpts pabConf txInfo (txMintScripts tx) (txRedeemers tx) (txMint tx)
      callCommand @w $ ShellArgs "cardano-cli" (opts ins mints) (const $ valBudget <> mintBudget)
    Left e -> return $ Left e
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
        , requiredSigners
        , ["--fee", showText . getLovelace . fromValue $ txFee tx]
        , mconcat
            [ ["--protocol-params-file", pabConf.pcProtocolParamsFile]
            , ["--out-file", txFilePath pabConf "raw" tx]
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
        , ["--tx-body-file", txFilePath pabConf "raw" tx]
        , signingKeyFiles
        , ["--out-file", txFilePath pabConf "signed" tx]
        ]

budgetFromConfig :: PABConfig -> ExBudget -> ExBudget
budgetFromConfig pabConf derivedBudget =
  maybe
    derivedBudget
    (\(steps, memory) -> ExBudget (ExCPU $ fromInteger steps) (ExMemory $ fromInteger memory))
    pabConf.pcForceBudget

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
          , ["--tx-file", txFilePath pabConf "signed" tx]
          , networkOpt pabConf
          ]
      )
      (const ())

txInOpts :: PABConfig -> TxInfo -> Set TxIn -> ([Text], ExBudget)
txInOpts pabConf txInfo =
  foldMap
    ( \(TxIn txOutRef txInType) ->
        let (opts, exBudget) = scriptInputs txOutRef txInType
         in (,exBudget) $
              mconcat
                [ ["--tx-in", txOutRefToCliArg txOutRef]
                , opts
                ]
    )
    . Set.toList
  where
    scriptInputs :: TxOutRef -> Maybe TxInType -> ([Text], ExBudget)
    scriptInputs txOutRef txInType =
      case txInType of
        Just (ConsumeScriptAddress validator redeemer datum) ->
          let scriptContext = ScriptContext txInfo $ Plutus.Spending txOutRef
              exBudget =
                budgetFromConfig pabConf $
                  fromRight mempty $
                    calculateExBudget
                      (Scripts.unValidatorScript validator)
                      [Plutus.getRedeemer redeemer, Plutus.getDatum datum, Plutus.toBuiltinData scriptContext]
           in (,exBudget) $
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
mintOpts :: PABConfig -> TxInfo -> Set Scripts.MintingPolicy -> Redeemers -> Value -> ([Text], ExBudget)
mintOpts pabConf txInfo mintingPolicies redeemers mintValue =
  let scriptOpts =
        foldMap
          ( \(idx, policy) ->
              let redeemerPtr = RedeemerPtr Mint idx
                  redeemer = Map.lookup redeemerPtr redeemers
                  curSymbol = Value.mpsSymbol $ Scripts.mintingPolicyHash policy
                  scriptContext = ScriptContext txInfo $ Plutus.Minting curSymbol
                  exBudget r =
                    budgetFromConfig pabConf $
                      fromRight mempty $
                        calculateExBudget
                          (Scripts.unMintingPolicyScript policy)
                          [Plutus.getRedeemer r, Plutus.toBuiltinData scriptContext]
                  toOpts r =
                    let budget = exBudget r
                     in (,budget) $
                          mconcat
                            [ ["--mint-script-file", policyScriptFilePath pabConf curSymbol]
                            , ["--mint-redeemer-file", redeemerJsonFilePath pabConf (Ledger.redeemerHash r)]
                            , ["--mint-execution-units", exBudgetToCliArg budget]
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
    tokenNameStr = decodeUtf8 $ hex $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

unsafeSerialiseAddress :: NetworkId -> Address -> Text
unsafeSerialiseAddress network address =
  case serialiseAddress <$> toCardanoAddress network address of
    Right a -> a
    Left _ -> error "Couldn't create address"

calculateExBudget :: Script -> [BuiltinData] -> Either Text ExBudget
calculateExBudget script builtinData = do
  -- TODO, pull this from the protocol, they're the same for now but may not always be
  modelParams <- maybeToRight "Cost model params invalid." Plutus.defaultCostModelParams
  let serialisedScript = ShortByteString.toShort $ LazyByteString.toStrict $ Codec.serialise script
      pData = map Plutus.builtinDataToData builtinData
  mapLeft showText $
    snd $
      Plutus.evaluateScriptCounting Plutus.Verbose modelParams serialisedScript pData

-- calculateExBudget :: Script -> [BuiltinData] -> Either Text ExBudget
-- calculateExBudget script builtinData = do
--   mapLeft showText $ fst <$> Scripts.evaluateScript (Scripts.applyArguments script $ Plutus.builtinDataToData <$> builtinData)

exBudgetToCliArg :: ExBudget -> Text
exBudgetToCliArg (ExBudget (ExCPU steps) (ExMemory memory)) =
  "(" <> showText steps <> "," <> showText memory <> ")"

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

-- -- TODO: There is some issue with this function, the generated wallet key is incorrect
-- toWalletKey :: Wallet -> Text
-- toWalletKey =
--   decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub
