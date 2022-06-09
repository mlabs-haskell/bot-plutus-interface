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
import Cardano.PlutusExample.Transfer (
  TransferParams,
  TransferSchema,
  transfer,
 )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Prelude

instance HasDefinitions TransferContracts where
  getDefinitions :: [TransferContracts]
  getDefinitions = []

  getSchema :: TransferContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @TransferSchema

  getContract :: (TransferContracts -> SomeBuiltin)
  getContract = \case
    Transfer payments ->
      SomeBuiltin $ transfer payments

newtype TransferContracts = Transfer TransferParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''TransferContracts)

main :: IO ()
main = do
  pabConf <-
    either error id
      <$> BotPlutusInterface.loadPABConfig "./pabConfig.value"
  BotPlutusInterface.runPAB @TransferContracts pabConf
