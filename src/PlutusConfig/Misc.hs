module PlutusConfig.Misc (
  serializeDeserialize,
  serializeDeserialize',
  isLoadSaveIdentity,
) where

import Config.Schema (HasSpec (anySpec), ValueSpec)
import PlutusConfig.Cardano.Api ()
import PlutusConfig.Cardano.Api.Shelley ()
import PlutusConfig.Ledger ()
import PlutusConfig.Types (ToValue, deserialize', serialize)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Prelude

serializeDeserialize :: (ToValue a, HasSpec a) => a -> Either String a
serializeDeserialize = serializeDeserialize' anySpec

serializeDeserialize' :: (ToValue a) => ValueSpec a -> a -> Either String a
serializeDeserialize' spec = deserialize' spec . serialize

isLoadSaveIdentity :: forall a. Eq a => (FilePath -> a -> IO ()) -> (FilePath -> IO (Either String a)) -> a -> IO Bool
isLoadSaveIdentity save load x = withSystemTempDirectory "Config-test" $ \path -> do
  let confFile = path </> "conf.value"
  save confFile x
  Right x' <- load confFile
  return $ x == x'
