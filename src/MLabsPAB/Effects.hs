{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module MLabsPAB.Effects (
  createDirectoryIfMissing,
  queryChainIndex,
  listDirectory,
  threadDelay,
  uploadDir,
  printLog,
  PABEffect (..),
  ShellArgs (..),
  handlePABEffect,
  readFileTextEnvelope,
  writeFileJSON,
  writeFileTextEnvelope,
  callCommand,
) where

import Cardano.Api (AsType, FileError, HasTextEnvelope, TextEnvelopeDescr, TextEnvelopeError)
import Cardano.Api qualified
import Control.Concurrent qualified as Concurrent
import Control.Monad (void, when)
import Control.Monad.Freer (Eff, LastMember, interpretM, type (~>))
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson qualified as JSON
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import MLabsPAB.ChainIndex (handleChainIndexReq)
import MLabsPAB.Types (CLILocation (..), LogLevel (..), PABConfig (..))
import Plutus.Contract.Effects (ChainIndexQuery, ChainIndexResponse)
import System.Directory qualified as Directory
import System.Process (readProcess)
import Prelude hiding (readFile)

data ShellArgs a = ShellArgs
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdOutParser :: String -> a
  }

instance Show (ShellArgs a) where
  show ShellArgs {cmdName, cmdArgs} = Text.unpack $ cmdName <> mconcat cmdArgs

data PABEffect r where
  CallCommand :: ShellArgs a -> PABEffect a
  CreateDirectoryIfMissing :: Bool -> FilePath -> PABEffect ()
  PrintLog :: LogLevel -> String -> PABEffect ()
  ThreadDelay :: Int -> PABEffect ()
  ReadFileTextEnvelope ::
    HasTextEnvelope a =>
    AsType a ->
    FilePath ->
    PABEffect (Either (FileError TextEnvelopeError) a)
  WriteFileJSON :: FilePath -> JSON.Value -> PABEffect (Either (FileError ()) ())
  WriteFileTextEnvelope ::
    HasTextEnvelope a =>
    FilePath ->
    Maybe TextEnvelopeDescr ->
    a ->
    PABEffect (Either (FileError ()) ())
  ListDirectory :: FilePath -> PABEffect [FilePath]
  UploadDir :: Text -> PABEffect ()
  QueryChainIndex :: ChainIndexQuery -> PABEffect ChainIndexResponse

handlePABEffect ::
  forall (effs :: [Type -> Type]).
  (LastMember IO effs) =>
  PABConfig ->
  Eff (PABEffect ': effs) ~> Eff effs
handlePABEffect pabConf =
  interpretM
    ( \case
        CallCommand shellArgs ->
          case pabConf.pcCliLocation of
            Local -> callLocalCommand shellArgs
            Remote ipAddr -> callRemoteCommand ipAddr shellArgs
        CreateDirectoryIfMissing createParents filePath ->
          Directory.createDirectoryIfMissing createParents filePath
        PrintLog logLevel txt -> printLog' pabConf.pcLogLevel logLevel txt
        ThreadDelay microSeconds -> Concurrent.threadDelay microSeconds
        ReadFileTextEnvelope asType filepath -> Cardano.Api.readFileTextEnvelope asType filepath
        WriteFileJSON filepath value -> Cardano.Api.writeFileJSON filepath value
        WriteFileTextEnvelope filepath envelopeDescr contents ->
          Cardano.Api.writeFileTextEnvelope filepath envelopeDescr contents
        ListDirectory filepath -> Directory.listDirectory filepath
        UploadDir dir ->
          case pabConf.pcCliLocation of
            Local -> pure ()
            Remote ipAddr ->
              void $ readProcess "scp" ["-r", Text.unpack dir, Text.unpack $ ipAddr <> ":$HOME"] ""
        QueryChainIndex query ->
          handleChainIndexReq pabConf query
    )

printLog' :: LogLevel -> LogLevel -> String -> IO ()
printLog' logLevelSetting msgLogLvl msg =
  when (logLevelSetting >= msgLogLvl) $ putStrLn msg

callLocalCommand :: forall (a :: Type). ShellArgs a -> IO a
callLocalCommand ShellArgs {cmdName, cmdArgs, cmdOutParser} =
  cmdOutParser <$> readProcess (Text.unpack cmdName) (map Text.unpack cmdArgs) ""

callRemoteCommand :: forall (a :: Type). Text -> ShellArgs a -> IO a
callRemoteCommand ipAddr ShellArgs {cmdName, cmdArgs, cmdOutParser} =
  cmdOutParser
    <$> readProcess
      "ssh"
      (map Text.unpack [ipAddr, Text.unwords $ "source ~/.bash_profile;" : cmdName : map quotes cmdArgs])
      ""
quotes :: Text -> Text
quotes str = "\"" <> str <> "\""

makeEffect ''PABEffect
