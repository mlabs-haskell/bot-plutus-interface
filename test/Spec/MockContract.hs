{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.MockContract (
  signingKey1,
  signingKey2,
  runContractPure,
  toSigningKeyFile,
  runContractPure',
  MockContractState (..),
  pubKey1,
  pubKey2,
  pubKey3,
  pkh1,
  pkh2,
  pkh3,
  pkh1',
  pkh2',
  pkh3',
  addr1,
  addr2,
  addr3,
  commandHistory,
  instanceUpdateHistory,
  logHistory,
  contractEnv,
  observableState,
  files,
  tip,
  utxos,
) where

import Cardano.Api (
  AsType,
  FileError (FileError, FileIOError),
  HasTextEnvelope,
  NetworkId (Mainnet),
  PaymentKey,
  SigningKey (PaymentSigningKey),
  TextEnvelope,
  TextEnvelopeDescr,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
  serialiseToTextEnvelope,
 )
import Cardano.Crypto.DSIGN (genKeyDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens (at, (%~), (&), (<|), (?~), (^.), (^..), _1)
import Control.Lens.TH (makeLenses)
import Control.Monad (join)
import Control.Monad.Freer (Eff, reinterpret2, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Pagination (pageOf)
import Control.Monad.Freer.State (State, get, modify, runState)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as ByteString
import Data.Default (Default (def))
import Data.Either.Combinators (fromRight, mapLeft)
import Data.Kind (Type)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra (first)
import Data.UUID qualified as UUID
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (IOError))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKey, PubKeyHash)
import Ledger.Scripts (DatumHash (DatumHash))
import Ledger.Tx (TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.TxId (TxId (TxId))
import Ledger.Value qualified as Value
import MLabsPAB.CardanoCLI (unsafeSerialiseAddress)
import MLabsPAB.Contract (handleContract)
import MLabsPAB.Effects (PABEffect (..), ShellArgs (..))
import MLabsPAB.Files qualified as Files
import MLabsPAB.Types (
  ContractEnvironment (..),
  ContractState (ContractState, csActivity, csObservableState),
  LogLevel (..),
  PABConfig (..),
 )
import NeatInterpolation (text)
import Plutus.ChainIndex.Types (BlockId (..), Tip (..))
import Plutus.Contract (Contract (Contract))
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import PlutusTx.Builtins (fromBuiltin)
import System.IO.Unsafe (unsafePerformIO)
import Wallet.Emulator (knownWallet)
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude

signingKey1, signingKey2, signingKey3 :: SigningKey PaymentKey
signingKey1 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 0
signingKey2 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 1
signingKey3 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 2

pubKey1, pubKey2, pubKey3 :: PubKey
pubKey1 = toPubKey signingKey1
pubKey2 = toPubKey signingKey2
pubKey3 = toPubKey signingKey3

pkh1, pkh2, pkh3 :: PubKeyHash
pkh1 = Ledger.pubKeyHash pubKey1
pkh2 = Ledger.pubKeyHash pubKey2
pkh3 = Ledger.pubKeyHash pubKey3

pkh1', pkh2', pkh3' :: Text
pkh1' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh1
pkh2' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh2
pkh3' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh3

addr1, addr2, addr3 :: Text
addr1 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress pkh1)
addr2 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress pkh2)
addr3 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress pkh3)

toPubKey :: SigningKey PaymentKey -> PubKey
toPubKey = Ledger.toPublicKey . fromRight (error "Impossible happened") . Files.fromCardanoPaymentKey

toSigningKeyFile :: FilePath -> SigningKey PaymentKey -> (FilePath, MockFile)
toSigningKeyFile signingKeyFileDir sKey =
  ( signingKeyFileDir ++ "/signing-key-" ++ show (Ledger.pubKeyHash (toPubKey sKey)) ++ ".skey"
  , TextEnvelopeFile $ serialiseToTextEnvelope Nothing sKey
  )

data MockFile
  = TextEnvelopeFile TextEnvelope
  | JsonFile JSON.Value
  | OtherFile Text
  deriving stock (Show, Eq)

data MockContractState w = MockContractState
  { _files :: Map FilePath MockFile
  , _commandHistory :: [Text]
  , _instanceUpdateHistory :: [Activity]
  , _observableState :: w
  , _logHistory :: [(LogLevel, String)]
  , _contractEnv :: ContractEnvironment w
  , _utxos :: [(TxOutRef, TxOut)]
  , _tip :: Tip
  }
  deriving stock (Show)

makeLenses ''MockContractState

instance Monoid w => Default (MockContractState w) where
  def =
    MockContractState
      { _files =
          Map.fromList $
            map
              (toSigningKeyFile "./signing-keys")
              [signingKey1, signingKey2, signingKey3]
      , _commandHistory = mempty
      , _instanceUpdateHistory = mempty
      , _observableState = mempty
      , _logHistory = mempty
      , _contractEnv = def
      , _utxos = []
      , _tip = Tip 1000 (BlockId "ab12") 4
      }

instance Monoid w => Default (ContractEnvironment w) where
  def =
    ContractEnvironment
      { cePABConfig = def {pcNetwork = Mainnet, pcOwnPubKeyHash = pkh1}
      , ceContractInstanceId = ContractInstanceId UUID.nil
      , ceContractState = unsafePerformIO $ newTVarIO def
      , ceWallet = knownWallet 1
      }

instance Monoid w => Default (ContractState w) where
  def = ContractState {csActivity = Active, csObservableState = mempty}

type MockContract w a = Eff '[Error Text, State (MockContractState w)] a

{- | Run the contract monad in a pure mock runner, and return a tuple of the contract result and
 the contract state
-}
runContractPure ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  (ToJSON w, Monoid w) =>
  Contract w s Text a ->
  MockContractState w ->
  (Either Text a, MockContractState w)
runContractPure contract initContractState =
  let (res, st) = runContractPure' contract initContractState
   in ( res
      , st
          & commandHistory %~ reverse
          & instanceUpdateHistory %~ reverse
          & logHistory %~ reverse
      )

runContractPure' ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  (ToJSON w, Monoid w) =>
  Contract w s Text a ->
  MockContractState w ->
  (Either Text a, MockContractState w)
runContractPure' (Contract effs) initContractState =
  first join $
    runPABEffectPure initContractState $
      handleContract (initContractState ^. contractEnv) effs

runPABEffectPure ::
  forall (a :: Type) (w :: Type).
  MockContractState w ->
  Eff '[PABEffect w] a ->
  (Either Text a, MockContractState w)
runPABEffectPure initState req =
  run (runState initState (runError (reinterpret2 go req)))
  where
    go :: forall (v :: Type). PABEffect w v -> MockContract w v
    go (CallCommand args) = mockCallCommand args
    go (CreateDirectoryIfMissing createParents filePath) =
      mockCreateDirectoryIfMissing createParents filePath
    go (PrintLog logLevel msg) = mockPrintLog logLevel msg
    go (UpdateInstanceState msg) = mockUpdateInstanceState msg
    go (LogToContract msg) = mockLogToContract msg
    go (ThreadDelay microseconds) = mockThreadDelay microseconds
    go (ReadFileTextEnvelope asType filepath) = mockReadFileTextEnvelope asType filepath
    go (WriteFileJSON filepath value) = mockWriteFileJSON filepath value
    go (WriteFileTextEnvelope filepath envelopeDescr contents) =
      mockWriteFileTextEnvelope filepath envelopeDescr contents
    go (ListDirectory dir) = mockListDirectory dir
    go (UploadDir dir) = mockUploadDir dir
    go (QueryChainIndex query) = mockQueryChainIndex query

mockCallCommand ::
  forall (w :: Type) (a :: Type).
  ShellArgs a ->
  MockContract w a
mockCallCommand ShellArgs {cmdName, cmdArgs, cmdOutParser} = do
  modify @(MockContractState w) (commandHistory %~ (cmdName <> " " <> Text.unwords cmdArgs <|))

  case (cmdName, cmdArgs) of
    ("cardano-cli", "query" : "utxo" : "--address" : addr : _) ->
      cmdOutParser <$> mockQueryUtxo addr
    ("cardano-cli", "transaction" : "calculate-min-required-utxo" : _) ->
      pure $ cmdOutParser "Lovelace 50"
    ("cardano-cli", "transaction" : "calculate-min-fee" : _) ->
      pure $ cmdOutParser "200 Lovelace"
    ("cardano-cli", "transaction" : "build-raw" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ OtherFile "TxBody")
        _ -> throwError @Text "Out file argument is missing"

      pure $ cmdOutParser ""
    ("cardano-cli", "transaction" : "build" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ OtherFile "TxBody")
        _ -> throwError @Text "Out file argument is missing"

      pure $ cmdOutParser ""
    ("cardano-cli", "transaction" : "sign" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ OtherFile "Tx")
        _ -> throwError @Text "Out file argument is missing"

      pure $ cmdOutParser ""
    ("cardano-cli", "transaction" : "submit" : _) ->
      pure $ cmdOutParser ""
    _ -> throwError @Text "Unknown command"

mockQueryUtxo :: forall (w :: Type). Text -> MockContract w String
mockQueryUtxo addr = do
  state <- get @(MockContractState w)

  let network = (state ^. contractEnv).cePABConfig.pcNetwork
  pure $
    mockQueryUtxoOut $
      filter
        ((==) addr . unsafeSerialiseAddress network . Ledger.txOutAddress . snd)
        (state ^. utxos)

mockQueryUtxoOut :: [(TxOutRef, TxOut)] -> String
mockQueryUtxoOut utxos' =
  Text.unpack $
    Text.unlines
      [ "                           TxHash                                 TxIx        Amount"
      , "--------------------------------------------------------------------------------------"
      , Text.unlines $
          map
            ( \(TxOutRef (TxId txId) txIx, TxOut _ val datumHash) ->
                let txId' = encodeByteString $ fromBuiltin txId
                    txIx' = Text.pack $ show txIx
                    amts = valueToUtxoOut val
                    datumHash' = case datumHash of
                      Nothing -> "TxOutDatumNone"
                      Just (DatumHash dh) ->
                        "TxDatumHash ScriptDataInAlonzoEra " <> encodeByteString (fromBuiltin dh)
                 in [text|${txId'}     ${txIx'}        ${amts} + ${datumHash'}"|]
            )
            utxos'
      ]

valueToUtxoOut :: Value.Value -> Text
valueToUtxoOut =
  Text.intercalate " + " . map stringifyValue' . Value.flattenValue
  where
    stringifyValue' (curSymbol, tokenName, tAmt) =
      let token =
            if curSymbol == Ada.adaSymbol
              then "lovelace"
              else
                let curSymbol' =
                      encodeByteString $
                        fromBuiltin $ Value.unCurrencySymbol curSymbol
                    tokenName' =
                      decodeUtf8 $
                        fromBuiltin $ Value.unTokenName tokenName
                 in if Text.null tokenName'
                      then curSymbol'
                      else [text|${curSymbol'}.${tokenName'}|]
       in Text.pack (show tAmt) <> " " <> token

mockCreateDirectoryIfMissing :: forall (w :: Type). Bool -> FilePath -> MockContract w ()
mockCreateDirectoryIfMissing _ _ = pure ()

mockPrintLog :: forall (w :: Type). LogLevel -> String -> MockContract w ()
mockPrintLog logLevel msg =
  modify @(MockContractState w) (logHistory %~ ((logLevel, msg) <|))

mockUpdateInstanceState :: forall (w :: Type). Activity -> MockContract w ()
mockUpdateInstanceState msg =
  modify @(MockContractState w) (instanceUpdateHistory %~ (msg <|))

mockLogToContract :: forall (w :: Type). (Monoid w) => w -> MockContract w ()
mockLogToContract msg =
  modify (observableState %~ (<> msg))

mockThreadDelay :: forall (w :: Type). Int -> MockContract w ()
mockThreadDelay _ = pure ()

mockReadFileTextEnvelope ::
  forall (w :: Type) (a :: Type).
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  MockContract w (Either (FileError TextEnvelopeError) a)
mockReadFileTextEnvelope ttoken filepath = do
  state <- get @(MockContractState w)

  pure $
    case state ^. files . at filepath of
      Nothing -> Left $ FileIOError filepath (IOError Nothing NoSuchThing "" "No such file in the MockContractState" Nothing Nothing)
      Just (TextEnvelopeFile te) ->
        mapLeft (FileError filepath) $ deserialiseFromTextEnvelope ttoken te
      Just _ -> Left $ FileError filepath $ TextEnvelopeAesonDecodeError "Invalid format."

mockWriteFileJSON :: forall (w :: Type). FilePath -> JSON.Value -> MockContract w (Either (FileError ()) ())
mockWriteFileJSON filepath value = do
  let fileContent = JsonFile value
  modify @(MockContractState w) (files . at filepath ?~ fileContent)

  pure $ Right ()

mockWriteFileTextEnvelope ::
  forall (w :: Type) (a :: Type).
  HasTextEnvelope a =>
  FilePath ->
  Maybe TextEnvelopeDescr ->
  a ->
  MockContract w (Either (FileError ()) ())
mockWriteFileTextEnvelope filepath descr content = do
  let fileContent = TextEnvelopeFile (serialiseToTextEnvelope descr content)
  modify @(MockContractState w) (files . at filepath ?~ fileContent)

  pure $ Right ()

mockListDirectory :: forall (w :: Type). FilePath -> MockContract w [FilePath]
mockListDirectory filepath = do
  state <- get @(MockContractState w)
  pure $ map (drop (length filepath + 1)) $ filter (filepath `isPrefixOf`) $ Map.keys (state ^. files)

mockUploadDir :: forall (w :: Type). Text -> MockContract w ()
mockUploadDir _ = pure ()

mockQueryChainIndex :: forall (w :: Type). ChainIndexQuery -> MockContract w ChainIndexResponse
mockQueryChainIndex = \case
  DatumFromHash _ ->
    -- pure $ DatumHashResponse Nothing
    throwError @Text "DatumFromHash is unimplemented"
  ValidatorFromHash _ ->
    -- pure $ ValidatorHashResponse Nothing
    throwError @Text "ValidatorFromHashis unimplemented"
  MintingPolicyFromHash _ ->
    -- pure $ MintingPolicyHashResponse Nothing
    throwError @Text "GetTip is unimplemented"
  StakeValidatorFromHash _ ->
    -- pure $ StakeValidatorHashResponse Nothing
    throwError @Text "StakeValidatorFromHash is unimplemented"
  RedeemerFromHash _ ->
    -- pure $ RedeemerHashResponse Nothing
    throwError @Text "RedeemerFromHash is unimplemented"
  TxOutFromRef txOutRef -> do
    state <- get @(MockContractState w)
    pure $ TxOutRefResponse $ Tx.fromTxOut =<< lookup txOutRef (state ^. utxos)
  TxFromTxId _ ->
    -- pure $ TxIdResponse Nothing
    throwError @Text "TxFromTxId is unimplemented"
  UtxoSetMembership _ ->
    throwError @Text "UtxoSetMembership is unimplemented"
  UtxoSetAtAddress pageQuery _ -> do
    state <- get @(MockContractState w)
    pure $
      UtxoSetAtResponse
        ( state ^. tip
        , pageOf pageQuery (Set.fromList (state ^. utxos ^.. traverse . _1))
        )
  UtxoSetWithCurrency pageQuery _ -> do
    state <- get @(MockContractState w)
    pure $
      UtxoSetAtResponse
        ( state ^. tip
        , pageOf pageQuery (Set.fromList (state ^. utxos ^.. traverse . _1))
        )
  GetTip ->
    throwError @Text "GetTip is unimplemented"
