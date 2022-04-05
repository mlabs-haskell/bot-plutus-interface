module Spec.PlutusConfig.Cardano.Api (tests) where

import Cardano.Api (makePraosNonce)
import PlutusConfig.Cardano.Api ()
import PlutusConfig.Cardano.Api.Shelley ()
import PlutusConfig.Ledger ()
import PlutusConfig.Types (deserialize, serialize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "Cardano.Api configuration"
    [ testPraosNonce
    ]

testPraosNonce :: TestTree
testPraosNonce =
  testGroup
    "PraosNonce serialize/deserialize"
    [ testCase
        "PraosNonce serialize"
        (serialize praosNonce @?= sPraosNonce)
    , testCase
        "PraosNonce deserialize"
        (deserialize sPraosNonce @?= Right praosNonce)
    ]
  where
    praosNonce = makePraosNonce "Some string"
    sPraosNonce = "\"33b058e9898a60f0fe95d41b92bcbff152a008785ff1b390fd594ff4fe282770\""
