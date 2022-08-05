{-# OPTIONS_GHC -Wno-unused-binds #-}

module Spec.BotPlutusInterface.CoinSelection (tests) where

import BotPlutusInterface.CoinSelection (selectTxIns, uniqueAssetClasses, valuesToVecs)
import BotPlutusInterface.Effects (PABEffect)
import Control.Lens (filtered, foldOf, folded, to, (^..))
import Data.Default (def)
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (Address (Address), PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Crypto (PubKeyHash)
import Ledger.Tx (TxIn (..), TxInType (..), TxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass, Value, leq)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Spec.MockContract (runPABEffectPure)
import Spec.RandomLedger
import Test.QuickCheck (Gen, Property, forAll, withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude

tests :: TestTree
tests =
  testGroup
    "BotPlutusInterface.CoinSelection"
    [ testProperty "Have All unique assetClasses" assertUniqueAssetClasses
    , testProperty "columns of vectors represent same assetClass" validValueVectors
    , testProperty "coin selection produces valid balanced Tx" validateBalancing
    , testCase "coin selection greedy Approx" greedyApprox
    ]

pkh1 :: PubKeyHash
pkh1 = Address.unPaymentPubKeyHash . Wallet.paymentPubKeyHash $ Wallet.knownMockWallet 1

addr1 :: Address
addr1 = Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing

txOutRef1, txOutRef2, txOutRef3, txOutRef4, txOutRef5 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2
txOutRef5 = TxOutRef "34d491e0596b3a04be6e3442ebf115b33900f26e5e415e5151f820778ba576ed" 0

utxo1, utxo2, utxo3, utxo4, utxo5 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, TxOut addr1 (Ada.lovelaceValueOf 1_400_000) Nothing)
utxo2 = (txOutRef2, TxOut addr1 (Ada.lovelaceValueOf 1_200_000) Nothing)
utxo3 = (txOutRef3, TxOut addr1 (Ada.lovelaceValueOf 900_000) Nothing)
utxo4 = (txOutRef4, TxOut addr1 (Ada.lovelaceValueOf 800_000 <> Value.singleton "11223344" "Token1" 500) Nothing)
utxo5 = (txOutRef5, TxOut addr1 (Ada.lovelaceValueOf 600_000 <> Value.singleton "44332211" "Token2" 500) Nothing)

txIn1, txIn2, txIn3, txIn4, txIn5 :: TxIn
txIn1 = TxIn txOutRef1 (Just ConsumePublicKeyAddress)
txIn2 = TxIn txOutRef2 (Just ConsumePublicKeyAddress)
txIn3 = TxIn txOutRef3 (Just ConsumePublicKeyAddress)
txIn4 = TxIn txOutRef4 (Just ConsumePublicKeyAddress)
txIn5 = TxIn txOutRef5 (Just ConsumePublicKeyAddress)

validAssetClasses :: Assertion
validAssetClasses =
  uniqueAssetClasses (map (txOutValue . snd) [utxo1, utxo2, utxo3, utxo4, utxo5])
    @?= Set.fromList
      [ Value.assetClass Ada.adaSymbol Ada.adaToken
      , Value.assetClass "44332211" "Token2"
      , Value.assetClass "11223344" "Token1"
      ]

testOutputValue1, testOutputValue2, testOutputValue3, testOutputValue4 :: Value
testOutputValue1 = Ada.lovelaceValueOf 1_000_000 <> Value.singleton "11223344" "Token1" 100
testOutputValue2 = Ada.lovelaceValueOf 1_000_000
testOutputValue3 =
  Ada.lovelaceValueOf 1_000_000 <> Value.singleton "11223344" "Token1" 100
    <> Value.singleton "44332211" "Token2" 50
testOutputValue4 = Ada.lovelaceValueOf 1_500_000

greedyApprox :: Assertion
greedyApprox = do
  let eresult =
        mapM
          ( fst
              . runPABEffectPure def
              . selectTxIns @() @'[PABEffect ()] mempty (Map.fromList [utxo1, utxo2, utxo3, utxo4, utxo5])
          )
          [testOutputValue1, testOutputValue2, testOutputValue3, testOutputValue4]

      expectedResults =
        map
          (Right . Set.fromList)
          [ [txIn2, txIn4]
          , [txIn2]
          , [txIn4, txIn5, txIn1]
          , [txIn1, txIn2]
          ]

  case eresult of
    Left e -> assertFailure (Text.unpack e)
    Right result -> result @?= expectedResults

assertUniqueAssetClasses :: Property
assertUniqueAssetClasses = withMaxSuccess 1000 (forAll uniqueAssetClassesGen validate)
  where
    validate :: (Set AssetClass, [TxOut]) -> Bool
    validate (allAssetClasses, utxos) =
      Set.isSubsetOf (uniqueAssetClasses $ map txOutValue utxos) allAssetClasses

    uniqueAssetClassesGen :: Gen (Set AssetClass, [TxOut])
    uniqueAssetClassesGen =
      do
        let numUniqueAssetClasses :: Int
            numUniqueAssetClasses = 30

            assetClassSampleSize :: Int
            assetClassSampleSize = 30

            numUTxOs :: Int
            numUTxOs = 10

        allAssetClasses <- randomAssetClasses numUniqueAssetClasses

        utxos <- randomTxOuts numUTxOs assetClassSampleSize allAssetClasses

        pure (allAssetClasses, utxos)

validValueVectors :: Property
validValueVectors = withMaxSuccess 1000 (forAll txOutsGen validate)
  where
    validate :: [TxOut] -> Bool
    validate utxos =
      let values :: [Value]
          values = map txOutValue utxos

          uniqueAcs :: Set AssetClass
          uniqueAcs = uniqueAssetClasses values

          valueCheck :: Value -> Vector Integer -> Bool
          valueCheck value vec =
            Vec.fromList (uniqueAcs ^.. folded . to (Value.assetClassValueOf value)) == vec

          valuesVec :: [Vector Integer]
          valuesVec = Vec.toList $ valuesToVecs uniqueAcs values
       in all (uncurry valueCheck) (zip values valuesVec)

    txOutsGen :: Gen [TxOut]
    txOutsGen =
      do
        let numUniqueAssetClasses :: Int
            numUniqueAssetClasses = 30

            assetClassSampleSize :: Int
            assetClassSampleSize = 30

            numUTxOs :: Int
            numUTxOs = 10

        allAssetClasses <- randomAssetClasses numUniqueAssetClasses
        randomTxOuts numUTxOs assetClassSampleSize allAssetClasses

validateBalancing :: Property
validateBalancing = withMaxSuccess 10000 (forAll balanceGen validate)
  where
    validate :: (TxOut, Map TxOutRef TxOut) -> Bool
    validate (txOutput, utxos) =
      let result :: (Either Text (Either Text (Set TxIn)))
          result =
            fst $
              runPABEffectPure def $
                selectTxIns @() @'[PABEffect ()] mempty utxos (txOutValue txOutput)

          isScriptOutput :: TxOut -> Bool
          isScriptOutput TxOut {txOutAddress = Address {addressCredential = ScriptCredential _}} = True
          isScriptOutput TxOut {txOutAddress = Address {addressCredential = PubKeyCredential _}} = False

          sufficientValue :: Bool
          sufficientValue =
            txOutValue txOutput
              `leq` foldOf (folded . filtered (not . isScriptOutput) . to txOutValue) utxos

          toTxOut :: TxIn -> TxOut
          toTxOut = fromJust . (`Map.lookup` utxos) . txInRef
       in case result of
            Left _ -> False
            Right eselectedTxIns
              | not sufficientValue -> isLeft eselectedTxIns
              | otherwise ->
                case eselectedTxIns of
                  Left err
                    | txOutValue txOutput == mempty -> True
                    | otherwise -> error (show err)
                  Right selectedTxIns ->
                    txOutValue txOutput `leq` foldOf (folded . to (txOutValue . toTxOut)) selectedTxIns

    balanceGen :: Gen (TxOut, Map TxOutRef TxOut)
    balanceGen =
      do
        let numUniqueAssetClasses :: Int
            numUniqueAssetClasses = 10

            assetClassSampleSize :: Int
            assetClassSampleSize = 30

            numUTxOs :: Int
            numUTxOs = 20

        allAcs <- randomAssetClasses numUniqueAssetClasses
        rTxOuts <- randomTxOuts numUTxOs assetClassSampleSize allAcs
        rTxOutRefs <- randomTxOutRefs 19

        let txOutput :: TxOut
            txOutput = head rTxOuts

            utxos :: Map TxOutRef TxOut
            utxos = Map.fromList $ zip (tail rTxOutRefs) (tail rTxOuts)

        pure (txOutput, utxos)
