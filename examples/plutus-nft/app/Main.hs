{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import BotPlutusInterface qualified
import BotPlutusInterface.Types (
  CLILocation (Local),
  HasDefinitions (..),
  LogLevel (Debug),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.PlutusExample.NFT
import Data.Aeson qualified as JSON
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Http))
import Prelude

instance HasDefinitions MintNFTContracts where
  getDefinitions :: [MintNFTContracts]
  getDefinitions = []

  getSchema :: MintNFTContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @NFTSchema

  getContract :: (MintNFTContracts -> SomeBuiltin)
  getContract = \case
    MintNFT p ->
      SomeBuiltin $
        mintNft p

newtype MintNFTContracts = MintNFT MintParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''MintNFTContracts)

main :: IO ()
main = do
  protocolParams <-
    fromMaybe (error "protocol.json file not found") . JSON.decode
      <$> LazyByteString.readFile "protocol.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 1097911063)
          , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
          , pcPort = 9080
          , pcProtocolParams = protocolParams
          , pcTipPollingInterval = 10_000_000
          , pcSlotConfig = def
          , pcOwnPubKeyHash = "3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75"
          , pcScriptFileDir = "./scripts"
          , pcSigningKeyFileDir = "./signing-keys"
          , pcTxFileDir = "./txs"
          , pcDryRun = False
          , pcLogLevel = Debug
          , pcProtocolParamsFile = "./protocol.json"
          , pcForceBudget = Just (1000, 1000)
          , pcEnableTxEndpoint = True
          , pcMetadataDir = "./metadata"
          }
  BotPlutusInterface.runPAB @MintNFTContracts pabConf
