module Spec.PlutusConfig.Ledger (tests) where

import PlutusConfig.Ledger ()
import PlutusConfig.Types (deserialize, serialize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wallet.API (PubKeyHash (..))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Ledeger configure"
    [ testPubKeyHash
    ]

testPubKeyHash :: TestTree
testPubKeyHash =
  testGroup
    "PubKeyHash serialize/deserialize"
    [ testCase
        "PubKeyHash serialize"
        (serialize hash @?= shash)
    , testCase
        "PubKeyHash deserialize"
        (deserialize shash @?= Right hash)
    ]
  where
    hash :: PubKeyHash
    hash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    shash = "\"0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546\""
