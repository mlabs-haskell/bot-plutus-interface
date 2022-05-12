{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import BotPlutusInterface qualified
import BotPlutusInterface.Config qualified as BotPlutusInterface
import BotPlutusInterface.Types (
  HasDefinitions (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Cardano.PlutusExample.Game (
  GameSchema,
  GuessParams,
  LockParams,
  guess,
  lock,
 )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Prelude

instance HasDefinitions GameContracts where
  getDefinitions :: [GameContracts]
  getDefinitions = []

  getSchema :: GameContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @GameSchema

  getContract :: (GameContracts -> SomeBuiltin)
  getContract = \case
    Lock params -> SomeBuiltin $ lock params
    Guess params -> SomeBuiltin $ guess params

data GameContracts = Lock LockParams | Guess GuessParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''GameContracts)

main :: IO ()
main = do
  pabConf <-
    either error id
      <$> BotPlutusInterface.loadPABConfig "./pabConfig.value"
  BotPlutusInterface.runPAB @GameContracts pabConf
