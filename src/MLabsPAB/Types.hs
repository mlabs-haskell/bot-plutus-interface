module MLabsPAB.Types (
  PABConfig (..),
  CLILocation (..),
  ContractEnvironment (..),
  HasDefinitions (..),
  FormSchema,
  FunctionSchema,
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.Api.Shelley (ProtocolParameters)
import Data.Default (Default (def))
import Data.Text (Text)
import Ledger (PubKey)
import Playground.Types (FunctionSchema)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Schema (FormSchema)
import Wallet.Emulator (Wallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

data PABConfig = PABConfig
  { -- | Calling the cli through ssh when set to Remote
    pcCliLocation :: !CLILocation
  , pcNetwork :: !NetworkId
  , pcProtocolParams :: !(Maybe ProtocolParameters)
  , -- | Directory name of the script and data files
    pcScriptFileDir :: !Text
  , -- | Directory name of the signing key files
    pcSigningKeyFileDir :: !Text
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    pcProtocolParamsFile :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    pcDryRun :: !Bool
  }

data ContractEnvironment = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceWallet :: Wallet
  , -- | TODO: We should get this from the wallet, once the integration works
    ceOwnPubKey :: PubKey
  , -- | TODO: We should be able acalculate this
    ceFees :: Integer
  , -- | TODO: We should be able acalculate this
    ceMinLovelaces :: Integer
  }

data CLILocation = Local | Remote Text

instance Default PABConfig where
  def =
    PABConfig
      { pcCliLocation = Local
      , pcNetwork = Testnet (NetworkMagic 42)
      , pcProtocolParams = Nothing
      , pcScriptFileDir = "result-scripts"
      , pcSigningKeyFileDir = "signing-keys"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      }
