{-# LANGUAGE NamedFieldPuns #-}

module MLabsPAB.CardanoCLI (
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
) where

import Cardano.Api.Shelley (NetworkId (Mainnet, Testnet), NetworkMagic (..), serialiseAddress)
import Codec.Serialise qualified as Codec
import Control.Monad.Freer (Eff, Member)
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft, maybeToRight)
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
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Crypto (PubKey, PubKeyHash)
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
utxosAt pabConf address =
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

calculateMinUtxo ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  UnbalancedTx ->
  Eff effs (Either Text Integer)
calculateMinUtxo pabConf UnbalancedTx {unBalancedTxTx} =
  callCommand
    ShellArgs
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["transaction", "calculate-min-required-utxo", "--alonzo-era"]
            , txOutOpts pabConf (txOutputs unBalancedTxTx)
            , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
            ]
      , cmdOutParser = mapLeft Text.pack . parseOnly UtxoParser.feeParser . Text.pack
      }

-- | Calculating fee for an unbalanced transaction
calculateMinFee ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  Tx ->
  Eff effs (Either Text Integer)
calculateMinFee pabConf tx =
  callCommand
    ShellArgs
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["transaction", "calculate-min-fee"]
            , ["--tx-body-file", "tx.raw"]
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

{- | Build a tx body and write it to disk
 If a fee if specified, it uses the build-raw command
-}
buildTx ::
  forall (effs :: [Type -> Type]).
  Member PABEffect effs =>
  PABConfig ->
  PubKeyHash ->
  BuildMode ->
  Tx ->
  Eff effs ()
buildTx pabConf ownPkh buildMode tx =
  callCommand $ ShellArgs "cardano-cli" opts (const ())
  where
    ownAddr = Ledger.pubKeyHashAddress ownPkh
    requiredSigners =
      concatMap
        (\pubKey -> ["--required-signer", signingKeyFilePath pabConf (Ledger.pubKeyHash pubKey)])
        (Map.keys (Ledger.txSignatures tx))
    opts =
      mconcat
        [ ["transaction", if isRawBuildMode buildMode then "build-raw" else "build", "--alonzo-era"]
        , txInOpts pabConf buildMode (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txOutputs tx)
        , mintOpts pabConf buildMode (txMintScripts tx) (txRedeemers tx) (txMint tx)
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
