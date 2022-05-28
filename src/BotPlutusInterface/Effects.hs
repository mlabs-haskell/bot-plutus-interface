{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.Effects (
  PABEffect (..),
  ShellArgs (..),
  handlePABEffect,
  createDirectoryIfMissing,
  createDirectoryIfMissingCLI,
  queryChainIndex,
  listDirectory,
  threadDelay,
  uploadDir,
  updateInstanceState,
  printLog,
  printBpiLog,
  handleContractLog,
  logToContract,
  readFileTextEnvelope,
  writeFileJSON,
  writeFileRaw,
  writeFileTextEnvelope,
  callCommand,
  estimateBudget,
  saveBudget,
) where

import BotPlutusInterface.ChainIndex (handleChainIndexReq)
import BotPlutusInterface.ExBudget qualified as ExBudget
import BotPlutusInterface.Types (
  BudgetEstimationError,
  CLILocation (..),
  ContractEnvironment,
  ContractState (ContractState),
  LogContext (BpiLog, ContractLog),
  LogLevel (..),
  TxBudget,
  TxFile,
  addBudget,
  LogsList (LogsList),
 )
import Cardano.Api (AsType, FileError (FileIOError), HasTextEnvelope, TextEnvelopeDescr, TextEnvelopeError)
import Cardano.Api qualified
import Control.Concurrent qualified as Concurrent
import Control.Lens ((^.))
import Control.Concurrent.STM (atomically, modifyTVar, modifyTVar', TVar)
import Control.Monad (void, when)
import Control.Monad.Freer (Eff, LastMember, Member, interpretM, reinterpret, send, subsume, type (~>))
import Control.Monad.Freer.Extras (LogMsg (LMessage))
import Control.Monad.Freer.Extras qualified as Freer
import Control.Monad.Trans.Except.Extra (handleIOExceptT, runExceptT)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Bifunctor (second)
import Data.ByteString qualified as ByteString
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger qualified
import Plutus.Contract.Effects (ChainIndexQuery, ChainIndexResponse)
import Plutus.PAB.Core.ContractInstance.STM (Activity)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as Render
import System.Directory qualified as Directory
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcess, readProcessWithExitCode)
import Prelude hiding (readFile)

data ShellArgs a = ShellArgs
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdOutParser :: String -> a
  }

instance Show (ShellArgs a) where
  show ShellArgs {cmdName, cmdArgs} = Text.unpack $ cmdName <> mconcat cmdArgs

data PABEffect (w :: Type) (r :: Type) where
  CallCommand :: ShellArgs a -> PABEffect w (Either Text a)
  CreateDirectoryIfMissing :: Bool -> FilePath -> PABEffect w ()
  -- Same as above but creates folder on the CLI machine, be that local or remote.
  CreateDirectoryIfMissingCLI :: Bool -> FilePath -> PABEffect w ()
  PrintLog :: LogContext -> LogLevel -> PP.Doc () -> PABEffect w ()
  UpdateInstanceState :: Activity -> PABEffect w ()
  LogToContract :: (ToJSON w, Monoid w) => w -> PABEffect w ()
  ThreadDelay :: Int -> PABEffect w ()
  ReadFileTextEnvelope ::
    HasTextEnvelope a =>
    AsType a ->
    FilePath ->
    PABEffect w (Either (FileError TextEnvelopeError) a)
  WriteFileJSON :: FilePath -> JSON.Value -> PABEffect w (Either (FileError ()) ())
  WriteFileRaw :: FilePath -> BuiltinByteString -> PABEffect w (Either (FileError ()) ())
  WriteFileTextEnvelope ::
    HasTextEnvelope a =>
    FilePath ->
    Maybe TextEnvelopeDescr ->
    a ->
    PABEffect w (Either (FileError ()) ())
  ListDirectory :: FilePath -> PABEffect w [FilePath]
  UploadDir :: Text -> PABEffect w ()
  QueryChainIndex :: ChainIndexQuery -> PABEffect w ChainIndexResponse
  EstimateBudget :: TxFile -> PABEffect w (Either BudgetEstimationError TxBudget)
  SaveBudget :: Ledger.TxId -> TxBudget -> PABEffect w ()

handlePABEffect ::
  forall (w :: Type) (effs :: [Type -> Type]).
  LastMember IO effs =>
  (Monoid w) =>
  ContractEnvironment w ->
  Eff (PABEffect w ': effs) ~> Eff effs
handlePABEffect contractEnv =
  interpretM
    ( \case
        CallCommand shellArgs ->
          case contractEnv.cePABConfig.pcCliLocation of
            Local -> callLocalCommand shellArgs
            Remote ipAddr -> callRemoteCommand ipAddr shellArgs
        CreateDirectoryIfMissing createParents filePath ->
          Directory.createDirectoryIfMissing createParents filePath
        CreateDirectoryIfMissingCLI createParents filePath ->
          case contractEnv.cePABConfig.pcCliLocation of
            Local -> Directory.createDirectoryIfMissing createParents filePath
            Remote ipAddr -> createDirectoryIfMissingRemote ipAddr createParents filePath
        PrintLog logCtx logLevel txt ->
          let logMsg = prettyLog logCtx logLevel txt in do
            printLog' contractEnv.cePABConfig.pcLogLevel logCtx logLevel logMsg
            when contractEnv.cePABConfig.pcCollectLogs $
              collectLog contractEnv.ceContractLogs logLevel logMsg
        UpdateInstanceState s -> do
          atomically $
            modifyTVar contractEnv.ceContractState $
              \(ContractState _ w) -> ContractState s w
        LogToContract w -> do
          atomically $
            modifyTVar contractEnv.ceContractState $
              \(ContractState s w') -> ContractState s (w' <> w)
        ThreadDelay microSeconds -> Concurrent.threadDelay microSeconds
        ReadFileTextEnvelope asType filepath -> Cardano.Api.readFileTextEnvelope asType filepath
        WriteFileJSON filepath value -> Cardano.Api.writeFileJSON filepath value
        WriteFileRaw filepath (BuiltinByteString value) ->
          runExceptT $
            handleIOExceptT (FileIOError filepath) $
              ByteString.writeFile filepath value
        WriteFileTextEnvelope filepath envelopeDescr contents ->
          Cardano.Api.writeFileTextEnvelope filepath envelopeDescr contents
        ListDirectory filepath -> Directory.listDirectory filepath
        UploadDir dir ->
          case contractEnv.cePABConfig.pcCliLocation of
            Local -> pure ()
            Remote ipAddr ->
              void $ readProcess "scp" ["-r", Text.unpack dir, Text.unpack $ ipAddr <> ":$HOME"] ""
        QueryChainIndex query ->
          handleChainIndexReq contractEnv.cePABConfig query
        EstimateBudget txPath ->
          ExBudget.estimateBudget contractEnv.cePABConfig txPath
        SaveBudget txId exBudget -> saveBudgetImpl contractEnv txId exBudget
    )

printLog' :: LogLevel -> LogContext -> LogLevel -> PP.Doc () -> IO ()
printLog' logLevelSetting msgCtx msgLogLvl msg =
  when (logLevelSetting >= msgLogLvl) $ putStrLn target
  where
    target =
      Render.renderString . layoutPretty defaultLayoutOptions $
        prettyLog msgCtx msgLogLvl msg

prettyLog :: LogContext -> LogLevel -> PP.Doc () -> PP.Doc ()
prettyLog msgCtx msgLogLvl msg = pretty msgCtx <+> pretty msgLogLvl <+> msg

collectLog :: TVar LogsList -> LogLevel -> PP.Doc () -> IO ()
collectLog logs msgLogLvl msg = atomically $ modifyTVar' logs (appendLog msgLogLvl msg)
  where 
    appendLog :: LogLevel -> PP.Doc () -> LogsList -> LogsList
    appendLog logLvl str (LogsList ls) = LogsList $ (logLvl, str) : ls


-- | Reinterpret contract logs to be handled by PABEffect later down the line.
handleContractLog :: forall w a effs. Member (PABEffect w) effs => Pretty a => Eff (LogMsg a ': effs) ~> Eff effs
handleContractLog x = subsume $ handleContractLogInternal @w x

handleContractLogInternal :: forall w a effs. Pretty a => Eff (LogMsg a ': effs) ~> Eff (PABEffect w ': effs)
handleContractLogInternal = reinterpret $ \case
  LMessage logMsg ->
    let msgContent = logMsg ^. Freer.logMessageContent
        msgLogLevel = toNativeLogLevel (logMsg ^. Freer.logLevel)
        msgPretty = pretty msgContent
     in printLog @w ContractLog msgLogLevel msgPretty
  where
    toNativeLogLevel Freer.Debug = Debug
    toNativeLogLevel Freer.Info = Info
    toNativeLogLevel Freer.Notice = Notice
    toNativeLogLevel Freer.Warning = Warn
    toNativeLogLevel Freer.Error = Error
    toNativeLogLevel Freer.Critical = Error
    toNativeLogLevel Freer.Alert = Error
    toNativeLogLevel Freer.Emergency = Error

callLocalCommand :: forall (a :: Type). ShellArgs a -> IO (Either Text a)
callLocalCommand ShellArgs {cmdName, cmdArgs, cmdOutParser} =
  second cmdOutParser <$> readProcessEither (Text.unpack cmdName) (map Text.unpack cmdArgs)

callRemoteCommand :: forall (a :: Type). Text -> ShellArgs a -> IO (Either Text a)
callRemoteCommand ipAddr ShellArgs {cmdName, cmdArgs, cmdOutParser} =
  second cmdOutParser
    <$> readProcessEither
      "ssh"
      (map Text.unpack [ipAddr, Text.unwords $ "source ~/.bash_profile;" : cmdName : map quotes cmdArgs])

createDirectoryIfMissingRemote :: Text -> Bool -> FilePath -> IO ()
createDirectoryIfMissingRemote ipAddr createParents path =
  void $ readProcessEither "ssh" $ catMaybes [Just $ Text.unpack ipAddr, Just "mkdir", pFlag, Just $ quotes path]
  where
    pFlag :: Maybe String
    pFlag = if createParents then Just "-p" else Nothing

quotes :: forall (a :: Type). (IsString a, Semigroup a) => a -> a
quotes str = fromString "\"" <> str <> fromString "\""

readProcessEither :: FilePath -> [String] -> IO (Either Text String)
readProcessEither path args =
  mapToEither <$> readProcessWithExitCode path args ""
  where
    mapToEither :: (ExitCode, String, String) -> Either Text String
    mapToEither (ExitSuccess, stdout, _) = Right stdout
    mapToEither (ExitFailure exitCode, _, stderr) =
      Left $ "ExitCode " <> Text.pack (show exitCode) <> ": " <> Text.pack stderr

saveBudgetImpl :: ContractEnvironment w -> Ledger.TxId -> TxBudget -> IO ()
saveBudgetImpl contractEnv txId budget =
  atomically $
    modifyTVar' contractEnv.ceContractStats (addBudget txId budget)

-- Couldn't use the template haskell makeEffect here, because it caused an OverlappingInstances problem.
-- For some reason, we need to manually propagate the @w@ type variable to @send@

callCommand ::
  forall (w :: Type) (a :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ShellArgs a ->
  Eff effs (Either Text a)
callCommand = send @(PABEffect w) . CallCommand

estimateBudget ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  TxFile ->
  Eff effs (Either BudgetEstimationError TxBudget)
estimateBudget = send @(PABEffect w) . EstimateBudget

createDirectoryIfMissing ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Bool ->
  FilePath ->
  Eff effs ()
createDirectoryIfMissing createParents path = send @(PABEffect w) $ CreateDirectoryIfMissing createParents path

createDirectoryIfMissingCLI ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Bool ->
  FilePath ->
  Eff effs ()
createDirectoryIfMissingCLI createParents path = send @(PABEffect w) $ CreateDirectoryIfMissingCLI createParents path

printLog ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  LogContext ->
  LogLevel ->
  PP.Doc () ->
  Eff effs ()
printLog logCtx logLevel msg = send @(PABEffect w) $ PrintLog logCtx logLevel msg

printBpiLog ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  LogLevel ->
  PP.Doc () ->
  Eff effs ()
printBpiLog = printLog @w BpiLog

updateInstanceState ::
  forall (w :: Type) (effs :: [Type -> Type]). Member (PABEffect w) effs => Activity -> Eff effs ()
updateInstanceState = send @(PABEffect w) . UpdateInstanceState

logToContract ::
  forall (w :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs, ToJSON w, Monoid w) =>
  w ->
  Eff effs ()
logToContract = send @(PABEffect w) . LogToContract

threadDelay ::
  forall (w :: Type) (effs :: [Type -> Type]). Member (PABEffect w) effs => Int -> Eff effs ()
threadDelay = send @(PABEffect w) . ThreadDelay

readFileTextEnvelope ::
  forall (w :: Type) (a :: Type) (effs :: [Type -> Type]).
  (Member (PABEffect w) effs, HasTextEnvelope a) =>
  AsType a ->
  FilePath ->
  Eff effs (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope teType path = send @(PABEffect w) $ ReadFileTextEnvelope teType path

writeFileJSON ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  FilePath ->
  JSON.Value ->
  Eff effs (Either (FileError ()) ())
writeFileJSON path val = send @(PABEffect w) $ WriteFileJSON path val

writeFileRaw ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  FilePath ->
  BuiltinByteString ->
  Eff effs (Either (FileError ()) ())
writeFileRaw path val = send @(PABEffect w) $ WriteFileRaw path val

writeFileTextEnvelope ::
  forall (w :: Type) (a :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  HasTextEnvelope a =>
  FilePath ->
  Maybe TextEnvelopeDescr ->
  a ->
  Eff effs (Either (FileError ()) ())
writeFileTextEnvelope path teDesc content = send @(PABEffect w) $ WriteFileTextEnvelope path teDesc content

listDirectory ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  FilePath ->
  Eff effs [FilePath]
listDirectory = send @(PABEffect w) . ListDirectory

uploadDir ::
  forall (w :: Type) (effs :: [Type -> Type]). Member (PABEffect w) effs => Text -> Eff effs ()
uploadDir = send @(PABEffect w) . UploadDir

queryChainIndex ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  ChainIndexQuery ->
  Eff effs ChainIndexResponse
queryChainIndex = send @(PABEffect w) . QueryChainIndex

saveBudget ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Ledger.TxId ->
  TxBudget ->
  Eff effs ()
saveBudget txId budget = send @(PABEffect w) $ SaveBudget txId budget
