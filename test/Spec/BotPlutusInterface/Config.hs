module Spec.BotPlutusInterface.Config (tests) where

import BotPlutusInterface.Config
import BotPlutusInterface.Types (PABConfig)
import Config.Schema
import Data.Default (def)
import Data.Either (isLeft)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

{- | Tests for PABConfig serialize/deserialize

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.Config"
    [ testCase
        "PABConfig serialize/deserialize"
        (serializeDeserialize def @?= Right (def :: PABConfig))
    , testRationalSerializeDeserialize
    ]

testRationalSerializeDeserialize :: TestTree
testRationalSerializeDeserialize =
  testGroup
    "Rational"
    [ testCase
        "Rational serialize"
        (serialize (1 % 2 :: Rational) @?= "\"1 % 2\"")
    , testCase
        "Rational deserialize with value error"
        (isLeft (deserialize' customRationalSpec "\"1 % 0\"") @? "should be Left")
    , testCase
        "Rational deserialize with value error"
        (isLeft (deserialize' customRationalSpec "\"1\"") @? "should be Left")
    , testCase
        "Rational serialize/deserialize"
        (serializeDeserialize' customRationalSpec (1 % 2) @?= Right (1 % 2 :: Rational))
    ]

serializeDeserialize :: (ToValue a, HasSpec a) => a -> Either String a
serializeDeserialize = serializeDeserialize' anySpec

serializeDeserialize' :: (ToValue a) => ValueSpec a -> a -> Either String a
serializeDeserialize' spec = deserialize' spec . serialize
