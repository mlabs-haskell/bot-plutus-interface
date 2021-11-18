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
  TextEnvelopeError,
  deserialiseFromTextEnvelope,
  serialiseToTextEnvelope,
 )
import Cardano.Crypto.DSIGN (genKeyDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Monad.Freer (Eff, reinterpret3, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.State (State, get, modify, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as ByteString
import Data.Default (Default (def))
import Data.Either.Combinators (fromRight, mapLeft, maybeToRight)
import Data.Kind (Type)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (IOError))
import Ledger qualified
import Ledger.Crypto (PubKey, PubKeyHash)
import Ledger.Tx (TxOutRef (TxOutRef))
import Ledger.TxId (TxId (TxId))
import MLabsPAB.CardanoCLI (unsafeSerialiseAddress)
import MLabsPAB.Contract (handleContract)
import MLabsPAB.Effects (PABEffect (..), ShellArgs (..))
import MLabsPAB.Files qualified as Files
import MLabsPAB.Types (ContractEnvironment (..), LogLevel (..), PABConfig (..))
import NeatInterpolation (text)
import Plutus.ChainIndex.Types (BlockId (..), Page (..), PageSize (..), Tip (..))
import Plutus.Contract (Contract (Contract))
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import PlutusTx.Builtins (fromBuiltin)
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

toSigningKeyFile :: FilePath -> SigningKey PaymentKey -> (String, TextEnvelope)
toSigningKeyFile signingKeyFileDir sKey =
  ( signingKeyFileDir ++ "/signing-key-" ++ show (Ledger.pubKeyHash (toPubKey sKey)) ++ ".skey"
  , serialiseToTextEnvelope Nothing sKey
  )

data MockContractState = MockContractState
  { files :: Map FilePath TextEnvelope
  , commandHistory :: [Text]
  , contractEnv :: ContractEnvironment
  , utxos :: [(TxOutRef, Int, [(Text, Int)])]
  , tip :: Tip
  }
  deriving stock (Show, Eq)

instance Default MockContractState where
  def =
    MockContractState
      { files =
          Map.fromList $
            map
              (toSigningKeyFile "signing-keys")
              [signingKey1, signingKey2, signingKey3]
      , commandHistory = mempty
      , contractEnv = def
      , utxos = []
      , tip = Tip 1000 (BlockId "ab12") 4
      }

instance Default ContractEnvironment where
  def =
    ContractEnvironment
      { cePABConfig = def {pcNetwork = Mainnet}
      , ceContractInstanceId = ContractInstanceId UUID.nil
      , ceWallet = knownWallet 1
      , ceOwnPubKey = pubKey1
      , ceFees = 200
      , ceMinLovelaces = 50
      }
type MockContract a = Eff '[Error Text, State MockContractState, Writer [String]] a

{- | Run the contract monad in a pure mock runner, and return a tuple of the contract result and
 the contract state
-}
runContractPure ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  (Monoid w) =>
  Contract w s Text a ->
  MockContractState ->
  (Either Text a, MockContractState)
runContractPure contract initContractState =
  let ((res, st), _) = runContractPure' contract initContractState
   in (fst =<< res, st {commandHistory = reverse st.commandHistory})

runContractPure' ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  (Monoid w) =>
  Contract w s Text a ->
  MockContractState ->
  ((Either Text (Either Text a, w), MockContractState), [String])
runContractPure' (Contract effs) initContractState =
  runPABEffectPure initContractState $ handleContract initContractState.contractEnv effs

runPABEffectPure ::
  forall (a :: Type).
  MockContractState ->
  Eff '[PABEffect] a ->
  ((Either Text a, MockContractState), [String])
runPABEffectPure initState req =
  run (runWriter (runState initState (runError (reinterpret3 go req))))
  where
    go :: PABEffect v -> MockContract v
    go (CallCommand args) = mockCallCommand args
    go (CreateDirectoryIfMissing createParents filePath) =
      mockCreateDirectoryIfMissing createParents filePath
    go (PrintLog logLevel msg) = mockPrintLog logLevel msg
    go (ThreadDelay microseconds) = mockThreadDelay microseconds
    go (ReadFileTextEnvelope asType filepath) = mockReadFileTextEnvelope asType filepath
    go (WriteFileJSON filepath value) = mockWriteFileJSON filepath value
    go (WriteFileTextEnvelope filepath envelopeDescr contents) =
      mockWriteFileTextEnvelope filepath envelopeDescr contents
    go (ListDirectory dir) = mockListDirectory dir
    go (UploadDir dir) = mockUploadDir dir
    go (QueryChainIndex query) = mockQueryChainIndex query

mockCallCommand ::
  forall (a :: Type).
  ShellArgs a ->
  MockContract a
mockCallCommand ShellArgs {cmdName, cmdArgs, cmdOutParser} = do
  tell $ map Text.unpack cmdArgs
  modify (\st -> st {commandHistory = cmdName <> " " <> Text.unwords cmdArgs : st.commandHistory})

  case (cmdName, cmdArgs) of
    ("cardano-cli", "query" : "utxo" : _) -> do
      state <- get @MockContractState

      pure $ cmdOutParser $ mockQueryUtxoOut state.utxos
    ("cardano-cli", "transaction" : "build" : _) ->
      pure $ cmdOutParser ""
    ("cardano-cli", "transaction" : "sign" : _) ->
      pure $ cmdOutParser ""
    ("cardano-cli", "transaction" : "submit" : _) ->
      pure $ cmdOutParser ""
    _ -> throwError @Text "Unknown command"

mockQueryUtxoOut :: [(TxOutRef, Int, [(Text, Int)])] -> String
mockQueryUtxoOut utxos =
  Text.unpack $
    Text.unlines
      [ "                           TxHash                                 TxIx        Amount"
      , "--------------------------------------------------------------------------------------"
      , Text.unlines $
          map
            ( \(TxOutRef (TxId txId) txIx, amt, tokens) ->
                let txId' = encodeByteString $ fromBuiltin txId
                    txIx' = Text.pack $ show txIx
                    amts =
                      Text.intercalate
                        " + "
                        ( Text.pack (show amt) <> " " <> "lovelace" :
                          map (\(tSymbol, tAmt) -> Text.pack (show tAmt) <> " " <> tSymbol) tokens
                        )
                 in [text|${txId'}     ${txIx'}        ${amts} + TxOutDatumNone"|]
            )
            utxos
      ]

mockCreateDirectoryIfMissing :: Bool -> FilePath -> MockContract ()
mockCreateDirectoryIfMissing _ _ = pure ()

mockPrintLog :: LogLevel -> String -> MockContract ()
mockPrintLog _ msg = tell [msg]

mockThreadDelay :: Int -> MockContract ()
mockThreadDelay _ = pure ()

mockReadFileTextEnvelope ::
  forall (a :: Type).
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  MockContract (Either (FileError TextEnvelopeError) a)
mockReadFileTextEnvelope ttoken filepath = do
  state <- get @MockContractState

  let maybeFile = Map.lookup filepath state.files

  pure $ do
    te <-
      maybeToRight
        (FileIOError filepath (IOError Nothing NoSuchThing "" "No such file in the MockContractState" Nothing Nothing))
        maybeFile
    mapLeft (FileError filepath) $ deserialiseFromTextEnvelope ttoken te

mockWriteFileJSON :: FilePath -> JSON.Value -> MockContract (Either (FileError ()) ())
mockWriteFileJSON _ _ = pure $ Right ()

mockWriteFileTextEnvelope ::
  forall (a :: Type).
  HasTextEnvelope a =>
  FilePath ->
  Maybe TextEnvelopeDescr ->
  a ->
  MockContract (Either (FileError ()) ())
mockWriteFileTextEnvelope filepath descr content = do
  modify (\st -> st {files = Map.insert filepath (serialiseToTextEnvelope descr content) st.files})

  pure $ Right ()

mockListDirectory :: FilePath -> MockContract [FilePath]
mockListDirectory filepath = do
  state <- get @MockContractState
  pure $ map (drop (length filepath + 1)) $ filter (filepath `isPrefixOf`) $ Map.keys state.files

mockUploadDir :: Text -> MockContract ()
mockUploadDir _ = pure ()

mockQueryChainIndex :: ChainIndexQuery -> MockContract ChainIndexResponse
mockQueryChainIndex = \case
  DatumFromHash _ ->
    pure $ DatumHashResponse Nothing
  ValidatorFromHash _ ->
    pure $ ValidatorHashResponse Nothing
  MintingPolicyFromHash _ ->
    pure $ MintingPolicyHashResponse Nothing
  StakeValidatorFromHash _ ->
    pure $ StakeValidatorHashResponse Nothing
  RedeemerFromHash _ ->
    pure $ RedeemerHashResponse Nothing
  TxOutFromRef _ ->
    pure $ TxOutRefResponse Nothing
  TxFromTxId _ ->
    pure $ TxIdResponse Nothing
  UtxoSetMembership _ ->
    throwError @Text "UtxoSetMembership is unimplemented"
  UtxoSetAtAddress _ -> do
    state <- get @MockContractState
    let txOutRefs = map (\(o, _, _) -> o) state.utxos
    pure $ UtxoSetAtResponse (state.tip, Page (PageSize 100) 1 1 txOutRefs)
  GetTip ->
    throwError @Text "GetTip is unimplemented"
