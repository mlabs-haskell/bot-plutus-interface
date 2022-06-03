{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import BotPlutusInterface qualified
import BotPlutusInterface.Config qualified as BotPlutusInterface
import BotPlutusInterface.Types (
  HasDefinitions (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Cardano.PlutusExample.NFT
import Data.Aeson.TH (defaultOptions, deriveJSON)
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
    MintNFT p ->
      SomeBuiltin $
        mintNft p

newtype MintNFTContracts = MintNFT MintParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''MintNFTContracts)

main :: IO ()
main = do
  pabConf <-
    either error id
      <$> BotPlutusInterface.loadPABConfig "./pabConfig.value"
  BotPlutusInterface.runPAB @MintNFTContracts pabConf
