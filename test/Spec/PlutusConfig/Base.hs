module Spec.PlutusConfig.Base (tests) where

import Data.Either (isLeft)
import Data.Ratio ((%))
import PlutusConfig.Base (customRationalSpec)
import PlutusConfig.Types (deserialize', serialize)
import Spec.PlutusConfig.Misc (serializeDeserialize')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "base library"
    [ testRationalSerializeDeserialize
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
        ( isLeft (deserialize' customRationalSpec "\"1 % 0\"")
            @? "should be Left"
        )
    , testCase
        "Rational deserialize with value error"
        ( isLeft (deserialize' customRationalSpec "\"1\"")
            @? "should be Left"
        )
    , testCase
        "Rational serialize/deserialize"
        ( serializeDeserialize' customRationalSpec (1 % 2)
            @?= Right (1 % 2 :: Rational)
        )
    ]
