{-# LANGUAGE NamedFieldPuns #-}

module Spec.NatLedger
  ( NatValue(PValue)
  , NatTxOut(NatTxOut, natTxOutAddress, natTxOutValue, natTxOutDatumHash)
  , NatTxOutRef(NatTxOutRef, natTxOutRefId, natTxRefIdx)
  , toLedgerTxOut
  , toLedgerTxOutRef
  , toLedgerValue
  )
  where

import BotPlutusInterface.Types ()

import Data.Bifunctor (second)
import GHC.Natural (Natural)
import Ledger.Tx (TxOut (..), TxOutRef (..), Address)
import Ledger.Value (Value(Value), CurrencySymbol, TokenName)
import PlutusTx.AssocMap qualified as AssocMap
import Plutus.V1.Ledger.Api (TxId, DatumHash)
import Test.QuickCheck (Arbitrary(arbitrary))
import Prelude

newtype NatValue = PValue { getNatValue :: AssocMap.Map CurrencySymbol
                                             (AssocMap.Map TokenName Natural)
                          } deriving newtype (Arbitrary, Show)

toLedgerValue :: NatValue -> Value
toLedgerValue = Value
              . AssocMap.fromList
              . map (second $ AssocMap.fromList
                            . map (second toInteger)
                            . AssocMap.toList)
              . AssocMap.toList . getNatValue

data NatTxOutRef = NatTxOutRef { natTxOutRefId :: TxId
                               , natTxRefIdx :: Natural
                               } deriving stock (Show)

instance Arbitrary NatTxOutRef where
  arbitrary = do
    txId <- arbitrary
    refIdx <- arbitrary
    return (NatTxOutRef txId refIdx)

toLedgerTxOutRef :: NatTxOutRef -> TxOutRef
toLedgerTxOutRef NatTxOutRef {natTxRefIdx, natTxOutRefId} = TxOutRef natTxOutRefId (toInteger natTxRefIdx)

data NatTxOut = NatTxOut { natTxOutAddress :: Address
                         , natTxOutValue   :: NatValue
                         , natTxOutDatumHash :: Maybe DatumHash
                         } deriving stock (Show)

instance Arbitrary NatTxOut where
  arbitrary = do
    addr <- arbitrary
    value <- arbitrary
    datumhash <- arbitrary
    return (NatTxOut addr value datumhash)

toLedgerTxOut :: NatTxOut -> TxOut
toLedgerTxOut NatTxOut{natTxOutAddress, natTxOutValue, natTxOutDatumHash}
  = TxOut natTxOutAddress (toLedgerValue natTxOutValue) natTxOutDatumHash
