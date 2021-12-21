{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.PlutusExample.NFT (
  NFTSchema,
  mintNft,
 )
import Data.Aeson qualified as JSON
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy qualified as LazyByteString
import Ledger.Value (TokenName)
import MLabsPAB qualified
import MLabsPAB.Types (
  CLILocation (Local),
  HasDefinitions (..),
  LogLevel (Debug),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Prelude

instance HasDefinitions MintNFTContracts where
  getDefinitions :: [MintNFTContracts]
  getDefinitions = []

  getSchema :: MintNFTContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @NFTSchema

  getContract :: (MintNFTContracts -> SomeBuiltin)
  getContract = \case
    MintNFT tokenName ->
      SomeBuiltin $
        mintNft tokenName

newtype MintNFTContracts = MintNFT TokenName
  deriving stock (Show)

$(deriveJSON defaultOptions ''MintNFTContracts)

main :: IO ()
main = do
  protocolParams <- JSON.decode <$> LazyByteString.readFile "protocol.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 42)
          , pcProtocolParams = protocolParams
          , pcScriptFileDir = "./scripts"
          , pcSigningKeyFileDir = "./keys"
          , pcTxFileDir = "./txs"
          , pcDryRun = True
          , pcLogLevel = Debug
          , pcProtocolParamsFile = "./protocol.json"
          }
  MLabsPAB.runPAB @MintNFTContracts pabConf
