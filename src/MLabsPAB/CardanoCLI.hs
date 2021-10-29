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
import Data.Aeson.Extras (encodeByteString)
import Data.Attoparsec.Text (parseOnly)
import Data.Either.Combinators (rightToMaybe)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
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
import MLabsPAB.Files (
  datumJsonFilePath,
  policyScriptFilePath,
  redeemerJsonFilePath,
  signingKeyFilePath,
  validatorScriptFilePath,
 )
import MLabsPAB.Types (
  CLILocation (..),
  PABConfig (..),
 )
import MLabsPAB.UtxoParser qualified as UtxoParser
import Plutus.Contract.CardanoAPI (toCardanoAddress)
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (fromBuiltin)
import System.Process (readProcess)
import Prelude

data ShellCommand a = ShellCommand
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdOutParser :: String -> a
  }

callCommand :: PABConfig -> ShellCommand a -> IO a
callCommand PABConfig {pcCliLocation} ShellCommand {cmdName, cmdArgs, cmdOutParser} =
  case pcCliLocation of
    Local -> cmdOutParser <$> readProcess (Text.unpack cmdName) (map Text.unpack cmdArgs) ""
    Remote serverIP ->
      cmdOutParser
        <$> readProcess
          "ssh"
          (map Text.unpack [serverIP, Text.unwords $ "source ~/.bash_profile;" : cmdName : cmdArgs])
          ""

-- | Upload script files via ssh
uploadFiles :: PABConfig -> Text -> IO ()
uploadFiles pabConf serverIP =
  mapM_
    uploadDir
    [ pabConf.pcScriptFileDir
    , pabConf.pcSigningKeyFileDir
    ]
  where
    uploadDir dir = readProcess "scp" ["-r", Text.unpack dir, Text.unpack $ serverIP <> ":$HOME"] ""

-- | Getting all available UTXOs at an address (all utxos are assumed to be PublicKeyChainIndexTxOut)
utxosAt :: PABConfig -> Address -> IO (Map TxOutRef ChainIndexTxOut)
utxosAt pabConf address = do
  callCommand
    pabConf
    ShellCommand
      { cmdName = "cardano-cli"
      , cmdArgs =
          mconcat
            [ ["query", "utxo"]
            , ["--address", unsafeSerialiseAddress pabConf address]
            , networkOpt pabConf
            ]
      , cmdOutParser =
          Map.fromList . mapMaybe (rightToMaybe . toUtxo) . drop 2 . Text.lines . Text.pack
      }
  where
    toUtxo :: Text -> Either String (TxOutRef, ChainIndexTxOut)
    toUtxo line = parseOnly (UtxoParser.utxoMapParser address) line

-- | Build a tx body and write it to disk
buildTx :: PABConfig -> PubKey -> Tx -> IO ()
buildTx pabConf ownPubKey tx =
  callCommand pabConf $ ShellCommand "cardano-cli" opts (const ())
  where
    ownAddr = Ledger.pubKeyHashAddress $ Ledger.pubKeyHash ownPubKey
    requiredSigners =
      concatMap
        (\pubKey -> ["--required-signer", signingKeyFilePath pabConf (Ledger.pubKeyHash pubKey)])
        (ownPubKey : Map.keys (Ledger.txSignatures tx))
    opts =
      mconcat
        [ ["transaction", "build", "--alonzo-era"]
        , txInOpts pabConf (txInputs tx)
        , txInCollateralOpts (txCollateral tx)
        , txOutOpts pabConf (txOutputs tx)
        , mintOpts pabConf (txMintScripts tx) (txRedeemers tx) (txMint tx)
        , mconcat
            [ ["--change-address", unsafeSerialiseAddress pabConf ownAddr]
            , requiredSigners
            , networkOpt pabConf
            , ["--protocol-params-file", pabConf.pcProtocolParamsFile]
            , ["--out-file", "tx.raw"]
            ]
        ]

-- Signs and writes a tx (uses the tx body written to disk as input)
signTx :: PABConfig -> [PubKey] -> IO ()
signTx pabConf pubKeys =
  callCommand pabConf $
    ShellCommand
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
submitTx :: PABConfig -> IO (Maybe Text)
submitTx pabConf =
  callCommand pabConf $
    ShellCommand
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
          , quotes $ valueToCliArg mintValue
          ]
        else []
    ]

txOutOpts :: PABConfig -> [TxOut] -> [Text]
txOutOpts pabConf =
  concatMap
    ( \TxOut {txOutAddress, txOutValue} ->
        [ "--tx-out"
        , quotes $
            Text.intercalate
              "+"
              [ unsafeSerialiseAddress pabConf txOutAddress
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
  | curSymbol == Ada.adaSymbol && name == Ada.adaToken = amountStr
  | otherwise =
    amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
  where
    amountStr = showText amount
    curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
    tokenNameStr = decodeUtf8 $ fromBuiltin $ unTokenName name

valueToCliArg :: Value -> Text
valueToCliArg val =
  Text.intercalate " + " $ map flatValueToCliArg $ sort $ Value.flattenValue val

quotes :: Text -> Text
quotes str = "\"" <> str <> "\""

unsafeSerialiseAddress :: PABConfig -> Address -> Text
unsafeSerialiseAddress pabConf address =
  case serialiseAddress <$> toCardanoAddress pabConf.pcNetwork address of
    Right a -> a
    Left _ -> error "Couldn't create address"

showText :: Show a => a -> Text
showText = Text.pack . show

-- -- TODO: There is some issue with this function, the generated wallet key is incorrect
-- toWalletKey :: Wallet -> Text
-- toWalletKey =
--   decodeUtf8 . convertToBase Base16 . hash @ByteString @Blake2b_160 . unXPub . walletXPub
