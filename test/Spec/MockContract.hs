{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.MockContract (
  -- Mock private and public keys etc.
  signingKey1,
  signingKey2,
  signingKey3,
  verificationKey1,
  verificationKey2,
  verificationKey3,
  toSigningKeyFile,
  toVerificationKeyFile,
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
  paymentPkh1,
  paymentPkh2,
  paymentPkh3,
  pkhAddr1,
  pkhAddr2,
  pkhAddr3,
  -- Test interpreter
  runContractPure,
  runContractPure',
  MockContractState (..),
  commandHistory,
  statsUpdates,
  instanceUpdateHistory,
  logHistory,
  contractEnv,
  observableState,
  files,
  tip,
  utxos,
  mockBudget,
) where

import BotPlutusInterface.CardanoCLI (unsafeSerialiseAddress)
import BotPlutusInterface.Contract (handleContract)
import BotPlutusInterface.Effects (PABEffect (..), ShellArgs (..))
import BotPlutusInterface.Files qualified as Files
import BotPlutusInterface.Types (
  BudgetEstimationError,
  ContractEnvironment (..),
  ContractState (ContractState, csActivity, csObservableState),
  LogLevel (..),
  PABConfig (..),
  TxBudget (TxBudget),
  TxFile,
 )
import Cardano.Api (
  AsType,
  FileError (FileError, FileIOError),
  HasTextEnvelope,
  Key (VerificationKey, getVerificationKey),
  NetworkId (Mainnet),
  PaymentKey,
  SigningKey (PaymentSigningKey),
  TextEnvelope (TextEnvelope, teDescription, teRawCBOR, teType),
  TextEnvelopeDescr,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
  getVerificationKey,
  serialiseToTextEnvelope,
 )
import Cardano.Crypto.DSIGN (genKeyDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Applicative (liftA2)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens (at, (%~), (&), (<|), (?~), (^.), (^..), _1)
import Control.Lens.TH (makeLenses)
import Control.Monad (join)
import Control.Monad.Freer (Eff, reinterpret2, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Pagination (pageOf)
import Control.Monad.Freer.State (State, get, modify, runState)
import Data.Aeson (Result (Success), ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as ByteString
import Data.Default (Default (def))
import Data.Either.Combinators (fromRight, mapLeft)
import Data.Hex (hex, unhex)
import Data.Kind (Type)
import Data.List (isPrefixOf, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
import Ledger.Slot (Slot (getSlot))
import Ledger.Tx (TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Tx qualified as Tx
import Ledger.TxId (TxId (TxId))
import Ledger.Value qualified as Value
import NeatInterpolation (text)
import Plutus.ChainIndex.Api (UtxosResponse (..))
import Plutus.ChainIndex.Tx (ChainIndexTx (..), ChainIndexTxOutputs (ValidTx))
import Plutus.ChainIndex.Types (BlockId (..), BlockNumber (unBlockNumber), Tip (..))
import Plutus.Contract (Contract (Contract))
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))
import PlutusTx.Builtins (fromBuiltin)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Prelude

signingKey1, signingKey2, signingKey3 :: SigningKey PaymentKey
signingKey1 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 0
signingKey2 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 1
signingKey3 = PaymentSigningKey $ genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 2

verificationKey1, verificationKey2, verificationKey3 :: VerificationKey PaymentKey
verificationKey1 = getVerificationKey signingKey1
verificationKey2 = getVerificationKey signingKey2
verificationKey3 = getVerificationKey signingKey3

pubKey1, pubKey2, pubKey3 :: PubKey
pubKey1 = skeyToPubKey signingKey1
pubKey2 = skeyToPubKey signingKey2
pubKey3 = skeyToPubKey signingKey3

pkh1, pkh2, pkh3 :: PubKeyHash
pkh1 = Ledger.pubKeyHash pubKey1
pkh2 = Ledger.pubKeyHash pubKey2
pkh3 = Ledger.pubKeyHash pubKey3

paymentPkh1, paymentPkh2, paymentPkh3 :: Ledger.PaymentPubKeyHash
paymentPkh1 = Ledger.PaymentPubKeyHash pkh1
paymentPkh2 = Ledger.PaymentPubKeyHash pkh2
paymentPkh3 = Ledger.PaymentPubKeyHash pkh3

pkhAddr1, pkhAddr2, pkhAddr3 :: Ledger.Address
pkhAddr1 = Ledger.pubKeyHashAddress paymentPkh1 Nothing
pkhAddr2 = Ledger.pubKeyHashAddress paymentPkh2 Nothing
pkhAddr3 = Ledger.pubKeyHashAddress paymentPkh3 Nothing

pkh1', pkh2', pkh3' :: Text
pkh1' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh1
pkh2' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh2
pkh3' = encodeByteString $ fromBuiltin $ Ledger.getPubKeyHash pkh3

addr1, addr2, addr3 :: Text
addr1 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress paymentPkh1 Nothing)
addr2 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress paymentPkh2 Nothing)
addr3 = unsafeSerialiseAddress Mainnet (Ledger.pubKeyHashAddress paymentPkh3 Nothing)

skeyToPubKey :: SigningKey PaymentKey -> PubKey
skeyToPubKey =
  Ledger.toPublicKey
    . Files.unDummyPrivateKey
    . either (error . Text.unpack) id
    . Files.skeyToDummyPrivKey

vkeyToPubKey :: VerificationKey PaymentKey -> PubKey
vkeyToPubKey =
  Ledger.toPublicKey
    . Files.unDummyPrivateKey
    . either (error . Text.unpack) id
    . Files.vkeyToDummyPrivKey

toSigningKeyFile :: FilePath -> SigningKey PaymentKey -> (FilePath, MockFile)
toSigningKeyFile signingKeyFileDir sKey =
  ( signingKeyFileDir ++ "/signing-key-" ++ show (Ledger.pubKeyHash (skeyToPubKey sKey)) ++ ".skey"
  , TextEnvelopeFile $ serialiseToTextEnvelope Nothing sKey
  )

toVerificationKeyFile :: FilePath -> VerificationKey PaymentKey -> (FilePath, MockFile)
toVerificationKeyFile signingKeyFileDir vKey =
  ( signingKeyFileDir ++ "/signing-key-" ++ show (Ledger.pubKeyHash (vkeyToPubKey vKey)) ++ ".vkey"
  , TextEnvelopeFile $ serialiseToTextEnvelope Nothing vKey
  )

data MockFile
  = TextEnvelopeFile TextEnvelope
  | JsonFile JSON.Value
  | OtherFile Text
  deriving stock (Show, Eq)

data MockContractState w = MockContractState
  { _files :: Map FilePath MockFile
  , _statsUpdates :: [String]
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
      , _statsUpdates = mempty
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
      , ceContractStats = unsafePerformIO $ newTVarIO mempty
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
  run (runState initState (runError (reinterpret2 (incSlot . go) req)))
  where
    go :: forall (v :: Type). PABEffect w v -> MockContract w v
    go (CallCommand args) = mockCallCommand args
    go (CreateDirectoryIfMissing createParents filePath) =
      mockCreateDirectoryIfMissing createParents filePath
    go (CreateDirectoryIfMissingCLI createParents filePath) =
      mockCreateDirectoryIfMissing createParents filePath
    go (PrintLog logLevel msg) = mockPrintLog logLevel msg
    go (UpdateInstanceState msg) = mockUpdateInstanceState msg
    go (LogToContract msg) = mockLogToContract msg
    go (ThreadDelay microseconds) = mockThreadDelay microseconds
    go (ReadFileTextEnvelope asType filepath) = mockReadFileTextEnvelope asType filepath
    go (WriteFileJSON filepath value) = mockWriteFileJSON filepath value
    go (WriteFileRaw filepath value) = mockWriteFileRaw filepath value
    go (WriteFileTextEnvelope filepath envelopeDescr contents) =
      mockWriteFileTextEnvelope filepath envelopeDescr contents
    go (ListDirectory dir) = mockListDirectory dir
    go (UploadDir dir) = mockUploadDir dir
    go (QueryChainIndex query) = mockQueryChainIndex query
    go (EstimateBudget file) = mockExBudget file
    go (SaveBudget txId budget) = mockSaveBudget txId budget
    incSlot :: forall (v :: Type). MockContract w v -> MockContract w v
    incSlot mc =
      mc <* modify @(MockContractState w) (tip %~ incTip)

    incTip TipAtGenesis = Tip 1 (BlockId "00") 0
    incTip Tip {tipSlot, tipBlockId, tipBlockNo} =
      Tip
        { tipSlot = tipSlot + 1
        , tipBlockId = tipBlockId
        , tipBlockNo = tipBlockNo
        }

mockCallCommand ::
  forall (w :: Type) (a :: Type).
  ShellArgs a ->
  MockContract w (Either Text a)
mockCallCommand ShellArgs {cmdName, cmdArgs, cmdOutParser} = do
  modify @(MockContractState w) (commandHistory %~ (cmdName <> " " <> Text.unwords cmdArgs <|))

  case (cmdName, cmdArgs) of
    ("cardano-cli", "query" : "tip" : _) ->
      Right . cmdOutParser <$> mockQueryTip
    ("cardano-cli", "query" : "utxo" : "--address" : addr : _) ->
      Right . cmdOutParser <$> mockQueryUtxo addr
    ("cardano-cli", "transaction" : "calculate-min-required-utxo" : _) ->
      pure $ Right $ cmdOutParser "Lovelace 50"
    ("cardano-cli", "transaction" : "calculate-min-fee" : rest) ->
      pure $ Right $ cmdOutParser $ show (fromMaybe 0 $ toFee rest) ++ " Lovelace"
    ("cardano-cli", "transaction" : "build-raw" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ TextEnvelopeFile dummyTxRawFile)
        _ -> throwError @Text "Out file argument is missing"

      pure $ Right $ cmdOutParser ""
    ("cardano-cli", "transaction" : "build" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ TextEnvelopeFile dummyTxRawFile)
        _ -> throwError @Text "Out file argument is missing"

      pure $ Right $ cmdOutParser ""
    ("cardano-cli", "transaction" : "sign" : args) -> do
      case drop 1 $ dropWhile (/= "--out-file") args of
        filepath : _ ->
          modify @(MockContractState w) (files . at (Text.unpack filepath) ?~ TextEnvelopeFile dummyTxSignedFile)
        _ -> throwError @Text "Out file argument is missing"

      pure $ Right $ cmdOutParser ""
    ("cardano-cli", "transaction" : "submit" : _) ->
      pure $ Right $ cmdOutParser ""
    ("mv", _) -> pure $ Right $ cmdOutParser ""
    (unsupportedCmd, unsupportedArgs) ->
      throwError @Text
        ("Unsupported command: " <> Text.intercalate " " (unsupportedCmd : unsupportedArgs))

toFee :: [Text] -> Maybe Integer
toFee (_ : _ : _ : inCount : _ : outCount : _) = (100 *) <$> liftA2 (+) (textRead inCount) (textRead outCount)
  where
    textRead = readMaybe . Text.unpack
toFee _ = Nothing

mockQueryTip :: forall (w :: Type). MockContract w String
mockQueryTip = do
  state <- get @(MockContractState w)

  let (slot, blockId, blockNo) =
        case state ^. tip of
          TipAtGenesis -> ("0", "00", "0")
          Tip {tipSlot, tipBlockId, tipBlockNo} ->
            ( Text.pack $ show $ getSlot tipSlot
            , decodeUtf8 $ getBlockId tipBlockId
            , Text.pack $ show $ unBlockNumber tipBlockNo
            )
  pure $
    Text.unpack
      [text|{
              "era": "Alonzo",
              "syncProgress": "100.00",
              "hash": "${blockId}",
              "epoch": 1,
              "slot": ${slot},
              "block": ${blockNo}
            }|]

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
                 in [text|${txId'}     ${txIx'}        ${amts} + ${datumHash'}|]
            )
            utxos'
      ]

mockBudget :: String
mockBudget = "Some budget"

mockSaveBudget :: forall (w :: Type). TxId -> TxBudget -> MockContract w ()
mockSaveBudget _ _ =
  modify @(MockContractState w) (statsUpdates %~ (mockBudget :))

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
                        hex $
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

mockWriteFileRaw :: forall (w :: Type). FilePath -> BuiltinByteString -> MockContract w (Either (FileError ()) ())
mockWriteFileRaw filepath (BuiltinByteString value) = do
  let fileContent = OtherFile $ encodeByteString value
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
  TxFromTxId txId -> do
    -- TODO: Track some kind of state here, add tests to ensure this works correctly
    -- For now, empty txs
    state <- get @(MockContractState w)
    let knownUtxos = state ^. utxos
    pure $
      TxIdResponse $
        Just $
          ChainIndexTx
            { _citxTxId = txId
            , _citxInputs = mempty
            , _citxOutputs = buildOutputsFromKnownUTxOs knownUtxos txId
            , _citxValidRange = Ledger.always
            , _citxData = mempty
            , _citxRedeemers = mempty
            , _citxScripts = mempty
            , _citxCardanoTx = Nothing
            }
  UtxoSetMembership _ ->
    throwError @Text "UtxoSetMembership is unimplemented"
  UtxoSetAtAddress pageQuery _ -> do
    state <- get @(MockContractState w)
    pure $
      UtxoSetAtResponse $
        UtxosResponse
          (state ^. tip)
          (pageOf pageQuery (Set.fromList (state ^. utxos ^.. traverse . _1)))
  UtxoSetWithCurrency pageQuery _ -> do
    state <- get @(MockContractState w)
    pure $
      UtxoSetAtResponse $
        UtxosResponse
          (state ^. tip)
          (pageOf pageQuery (Set.fromList (state ^. utxos ^.. traverse . _1)))
  TxsFromTxIds ids -> do
    -- TODO: Track some kind of state here, add tests to ensure this works correctly
    -- For now, empty txs
    state <- get @(MockContractState w)
    let knownUtxos = state ^. utxos
    pure . TxIdsResponse . (<$> ids) $ \txId ->
      ChainIndexTx
        { _citxTxId = txId
        , _citxInputs = mempty
        , _citxOutputs = buildOutputsFromKnownUTxOs knownUtxos txId
        , _citxValidRange = Ledger.always
        , _citxData = mempty
        , _citxRedeemers = mempty
        , _citxScripts = mempty
        , _citxCardanoTx = Nothing
        }
  TxoSetAtAddress _ _ ->
    throwError @Text "TxoSetAtAddress is unimplemented"
  GetTip ->
    throwError @Text "GetTip is unimplemented"

-- | Fills in gaps of inputs with garbage TxOuts, so that the indexes we know about are in the correct positions
buildOutputsFromKnownUTxOs :: [(TxOutRef, TxOut)] -> TxId -> ChainIndexTxOutputs
buildOutputsFromKnownUTxOs knownUtxos txId = ValidTx $ fillGaps sortedRelatedRefs 0
  where
    sortedRelatedRefs = sortOn (Tx.txOutRefIdx . fst) $ filter ((== txId) . Tx.txOutRefId . fst) knownUtxos
    fillGaps :: [(TxOutRef, TxOut)] -> Integer -> [TxOut]
    fillGaps [] _ = []
    fillGaps (out@(TxOutRef _ n', txOut) : outs) n
      | n' == n = txOut : fillGaps outs (n + 1)
      | otherwise = defTxOut : fillGaps (out : outs) (n + 1)
    defTxOut = TxOut (Ledger.Address (PubKeyCredential "") Nothing) mempty Nothing

mockExBudget ::
  forall (w :: Type).
  TxFile ->
  MockContract w (Either BudgetEstimationError TxBudget)
mockExBudget _ = pure . Right $ TxBudget inBudgets policyBudgets
  where
    inBudgets = Map.singleton (TxOutRef txId 1) someBudget
    policyBudgets = Map.singleton policy someBudget

    someBudget = Ledger.ExBudget (Ledger.ExCPU 500000) (Ledger.ExMemory 2000)

    txId =
      let txId' =
            JSON.fromJSON $
              JSON.object
                ["getTxId" JSON..= ("e406b0cf676fc2b1a9edb0617f259ad025c20ea6f0333820aa7cef1bfe7302e5" :: Text)]
       in case txId' of
            Success tid -> tid
            _ -> error "Could not parse TxId"

    policy =
      let policy' =
            JSON.fromJSON . JSON.String $
              "648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198"
       in case policy' of
            Success p -> p
            _ -> error "Could not parse MintingPolicyHash"
dummyTxRawFile :: TextEnvelope
dummyTxRawFile =
  TextEnvelope
    { teType = "TxBodyAlonzo"
    , teDescription = ""
    , teRawCBOR = fromRight (error "failed to unpack CBOR hex") $ unhex "86a500848258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960182582076ed2fcda860de2cbacd0f3a169058fa91eff47bc1e1e5b6d84497159fbc9300008258209405c89393ba84b14bf8d3e7ed4788cc6e2257831943b58338bee8d37a3668fc00825820a1be9565ccac4a04d2b5bf0d0167196ae467da0d88161c9c827fbe76452b24ef000d8182582076ed2fcda860de2cbacd0f3a169058fa91eff47bc1e1e5b6d84497159fbc930000018482581d600f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a3b8cc4a582581d600f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e1a000d062782581d606696936bb8ae24859d0c2e4d05584106601f58a5e9466282c8561b88821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e1282581d60981fc565bcf0c95c0cfa6ee6693875b60d529d87ed7082e9bf03c6a4821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e0f021a000320250e81581c0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975469fff8080f5f6"
    }

dummyTxSignedFile :: TextEnvelope
dummyTxSignedFile =
  TextEnvelope
    { teType = "Tx AlonzoEra"
    , teDescription = ""
    , teRawCBOR = fromRight (error "failed to unpack CBOR hex") $ unhex "84a500848258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960182582076ed2fcda860de2cbacd0f3a169058fa91eff47bc1e1e5b6d84497159fbc9300008258209405c89393ba84b14bf8d3e7ed4788cc6e2257831943b58338bee8d37a3668fc00825820a1be9565ccac4a04d2b5bf0d0167196ae467da0d88161c9c827fbe76452b24ef000d8182582076ed2fcda860de2cbacd0f3a169058fa91eff47bc1e1e5b6d84497159fbc930000018482581d600f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a3b8cc4a582581d600f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e1a000d062782581d606696936bb8ae24859d0c2e4d05584106601f58a5e9466282c8561b88821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e1282581d60981fc565bcf0c95c0cfa6ee6693875b60d529d87ed7082e9bf03c6a4821a00150bd0a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14974657374546f6b656e0f021a000320250e81581c0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546a10081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b65840295a93849a67cecabb8286e561c407b6bd49abf8d2da8bfb821105eae4d28ef0ef1b9ee5e8abb8fd334059f3dfc78c0a65e74057a2dc8d1d12e46842abea600ff5f6"
    }
