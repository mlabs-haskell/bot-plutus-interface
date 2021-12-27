module MLabsPAB.Types (
  PABConfig (..),
  CLILocation (..),
  LogLevel (..),
  ContractEnvironment (..),
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.Api.Shelley (ProtocolParameters)
import Data.Default (Default (def))
import Data.Text (Text)
import Ledger (PubKeyHash)
import Network.Wai.Handler.Warp (Port)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Wallet.Emulator (Wallet)
import Wallet.Types (ContractInstanceId (..))
import Prelude

data PABConfig = PABConfig
  { -- | Calling the cli through ssh when set to Remote
    pcCliLocation :: !CLILocation
  , pcChainIndexUrl :: !BaseUrl
  , pcNetwork :: !NetworkId
  , pcProtocolParams :: !(Maybe ProtocolParameters)
  , -- | Directory name of the script and data files
    pcScriptFileDir :: !Text
  , -- | Directory name of the signing key files
    pcSigningKeyFileDir :: !Text
  , -- | Directory name of the transaction files
    pcTxFileDir :: !Text
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    pcProtocolParamsFile :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    pcDryRun :: !Bool
  , pcLogLevel :: !LogLevel
  , pcOwnPubKeyHash :: PubKeyHash
  , pcPort :: !Port
  }
  deriving stock (Show, Eq)

data ContractEnvironment = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceWallet :: Wallet
  }
  deriving stock (Show, Eq)

data CLILocation = Local | Remote Text
  deriving stock (Show, Eq)

data LogLevel = Error | Warn | Notice | Info | Debug
  deriving stock (Eq, Ord, Show)

instance Default PABConfig where
  def =
    PABConfig
      { pcCliLocation = Local
      , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
      , pcNetwork = Testnet (NetworkMagic 42)
      , pcProtocolParams = Nothing
      , pcScriptFileDir = "./result-scripts"
      , pcSigningKeyFileDir = "./signing-keys"
      , pcTxFileDir = "./txs"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      , pcLogLevel = Info
      , pcOwnPubKeyHash = ""
      , pcPort = 9080
      }
