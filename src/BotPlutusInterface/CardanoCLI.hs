{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module BotPlutusInterface.CardanoCLI (
  BuildMode (..),
  submitTx,
  calculateMinUtxo,
  calculateMinFee,
  buildTx,
  signTx,
  uploadFiles,
  validatorScriptFilePath,
  unsafeSerialiseAddress,
  policyScriptFilePath,
  utxosAt,
  queryTip,
) where

import BotPlutusInterface.Effects (PABEffect, ShellArgs (..), callCommand, printLog, uploadDir)
import BotPlutusInterface.Files (
  DummyPrivKey (FromSKey, FromVKey),
  datumJsonFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  signingKeyFilePath,
  txFilePath,
  validatorScriptFilePath,
 )
import BotPlutusInterface.Types (LogLevel (Warn), PABConfig, Tip)
import BotPlutusInterface.UtxoParser qualified as UtxoParser
import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Codec.Serialise qualified as Codec
import Control.Monad (join)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.ByteString.Short qualified as ShortByteString
import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Hex (hex)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
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
 )
import Ledger.TxId (TxId (..))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Plutus.Contract.CardanoAPI (toCardanoAddress)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol (..),
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  Script,
  TokenName (..),
 )
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx.Builtins (fromBuiltin)
import Prelude

-- | Upload script files to remote server
uploadFiles ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Eff effs ()
uploadFiles pabConf =
  mapM_
    (uploadDir @w)
    [ pabConf.pcScriptFileDir
    , pabConf.pcSigningKeyFileDir
    ]

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

data BuildMode = BuildRaw Integer | BuildAuto
  deriving stock (Show)

isRawBuildMode :: BuildMode -> Bool
isRawBuildMode (BuildRaw _) = True
isRawBuildMode _ = False

-- | Build a tx body and write it to disk
buildTx ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  PABConfig ->
  Map PubKeyHash DummyPrivKey ->
  PubKeyHash ->
  BuildMode ->
  Tx ->
  Eff effs (Either Text ())
buildTx pabConf privKeys ownPkh buildMode tx =
  callCommand @w $ ShellArgs "cardano-cli" opts (const ())
  where
    ownAddr = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash ownPkh) Nothing
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
    opts =
      mconcat
        [ ["transaction", if isRawBuildMode buildMode then "build-raw" else "build", "--alonzo-era"]
        , txInOpts pabConf buildMode (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txData tx) (txOutputs tx)
        , mintOpts pabConf buildMode (txMintScripts tx) (txRedeemers tx) (txMint tx)
        , validRangeOpts (txValidRange tx)
        , requiredSigners
        , case buildMode of
            BuildRaw fee -> ["--fee", showText fee]
            BuildAuto ->
              mconcat
                [ ["--change-address", unsafeSerialiseAddress pabConf.pcNetwork ownAddr]
                , networkOpt pabConf
                ]
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
  Map PubKeyHash DummyPrivKey ->
  Tx ->
  [PubKey] ->
  Eff effs (Either Text ())
signTx pabConf privKeys tx pubKeys =
  let skeys = Map.filter (\case FromSKey _ -> True; FromVKey _ -> False) privKeys
   in if all ((`Map.member` skeys) . Ledger.pubKeyHash) pubKeys
        then callCommand @w $ ShellArgs "cardano-cli" opts (const ())
        else do
          let err =
                Text.unlines
                  [ "Not all required signatures have signing key files. Please sign and submit the tx manually:"
                  , "Tx file: " <> txFilePath pabConf "raw" tx
                  , "Signatories (pkh): "
                      <> Text.unwords
                        (map (encodeByteString . fromBuiltin . getPubKeyHash . Ledger.pubKeyHash) pubKeys)
                  ]
          printLog @w Warn (Text.unpack err)
          pure $ Left err
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

txInOpts :: PABConfig -> BuildMode -> Set TxIn -> [Text]
txInOpts pabConf buildMode =
  concatMap
    ( \(TxIn txOutRef txInType) ->
        mconcat
          [ ["--tx-in", txOutRefToCliArg txOutRef]
          , case txInType of
              Just (ConsumeScriptAddress validator redeemer datum) ->
                let exBudget =
                      fromRight (ExBudget (ExCPU 0) (ExMemory 0)) $
                        calculateExBudget
                          (Scripts.unValidatorScript validator)
                          [Plutus.getRedeemer redeemer, Plutus.getDatum datum]
                 in mconcat
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
                      , if isRawBuildMode buildMode
                          then ["--tx-in-execution-units", exBudgetToCliArg exBudget]
                          else []
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
mintOpts :: PABConfig -> BuildMode -> Set Scripts.MintingPolicy -> Redeemers -> Value -> [Text]
mintOpts pabConf buildMode mintingPolicies redeemers mintValue =
  mconcat
    [ mconcat $
        concatMap
          ( \(idx, policy) ->
              let redeemerPtr = RedeemerPtr Mint idx
                  redeemer = Map.lookup redeemerPtr redeemers
                  curSymbol = Value.mpsSymbol $ Scripts.mintingPolicyHash policy
                  exBudget r =
                    fromRight (ExBudget (ExCPU 0) (ExMemory 0)) $
                      calculateExBudget
                        (Scripts.unMintingPolicyScript policy)
                        [Plutus.getRedeemer r]
                  toOpts r =
                    [ ["--mint-script-file", policyScriptFilePath pabConf curSymbol]
                    , ["--mint-redeemer-file", redeemerJsonFilePath pabConf (Ledger.redeemerHash r)]
                    , if isRawBuildMode buildMode
                        then ["--mint-execution-units", exBudgetToCliArg (exBudget r)]
                        else []
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
  modelParams <- maybeToRight "Cost model params invalid." Plutus.defaultCostModelParams
  let serialisedScript = ShortByteString.toShort $ LazyByteString.toStrict $ Codec.serialise script
  let pData = map Plutus.builtinDataToData builtinData
  mapLeft showText $
    snd $
      Plutus.evaluateScriptCounting Plutus.Verbose modelParams serialisedScript pData

exBudgetToCliArg :: ExBudget -> Text
exBudgetToCliArg (ExBudget (ExCPU steps) (ExMemory memory)) =
  "(" <> showText steps <> "," <> showText memory <> ")"

showText :: forall (a :: Type). Show a => a -> Text
showText = Text.pack . show

-- -- TODO: There is some issue with this function, the generated wallet key is incorrect
-- toWalletKey :: Wallet -> Text
-- toWalletKey =
--   decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub
