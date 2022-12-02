{-# OPTIONS_GHC -Wno-unused-binds #-}

module Spec.BotPlutusInterface.CoinSelection (tests) where

import BotPlutusInterface.CoinSelection (selectTxIns, uniqueAssetClasses, valuesToVecs)
import BotPlutusInterface.Effects (PABEffect)
import BotPlutusInterface.Helpers (addressTxOut, lovelaceValueOf, unsafeToCardanoAddressInEra, unsafeValueOf)
import Cardano.Api qualified as CApi
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
import Ledger (txOutValue)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as Wallet
import Ledger.Crypto (PubKeyHash)
import Ledger.Tx (TxInput (TxInput, txInputRef), TxInputType (TxConsumePublicKeyAddress), TxOut (TxOut), TxOutRef (TxOutRef))
import Ledger.Value (AssetClass, Value, leq)
import Ledger.Value qualified as Value
import Spec.MockContract (runPABEffectPure, testingNetwork)
import Spec.RandomLedger
import Test.QuickCheck (Gen, Property, forAll, withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Wallet.API qualified as WAPI
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

addr1 :: CApi.AddressInEra CApi.BabbageEra
addr1 = unsafeToCardanoAddressInEra testingNetwork $ Ledger.pubKeyHashAddress (PaymentPubKeyHash pkh1) Nothing

txOutRef1, txOutRef2, txOutRef3, txOutRef4, txOutRef5 :: TxOutRef
txOutRef1 = TxOutRef "384de3f29396fdf687551e3f9e05bd400adcd277720c71f1d2b61f17f5183e51" 0
txOutRef2 = TxOutRef "52a003b3f4956433429631afe4002f82a924a5a7a891db7ae1f6434797a57dff" 1
txOutRef3 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 0
txOutRef4 = TxOutRef "d8a5630a9d7e913f9d186c95e5138a239a4e79ece3414ac894dbf37280944de3" 2
txOutRef5 = TxOutRef "34d491e0596b3a04be6e3442ebf115b33900f26e5e415e5151f820778ba576ed" 0

currencySymbol1, currencySymbol2 :: Ledger.CurrencySymbol
currencySymbol1 = "11111111111111111111111111111111111111111111111111111111"
currencySymbol2 = "22222222222222222222222222222222222222222222222222222222"

utxo1, utxo2, utxo3, utxo4, utxo5 :: (TxOutRef, TxOut)
utxo1 = (txOutRef1, addressTxOut addr1 $ lovelaceValueOf 1_400_000)
utxo2 = (txOutRef2, addressTxOut addr1 $ lovelaceValueOf 1_200_000)
utxo3 = (txOutRef3, addressTxOut addr1 $ lovelaceValueOf 900_000)
utxo4 = (txOutRef4, addressTxOut addr1 $ lovelaceValueOf 800_000 <> unsafeValueOf currencySymbol1 "Token1" 500)
utxo5 = (txOutRef5, addressTxOut addr1 $ lovelaceValueOf 600_000 <> unsafeValueOf currencySymbol2 "Token2" 500)

txInput1, txInput2, txInput3, txInput4, txInput5 :: TxInput
txInput1 = TxInput txOutRef1 TxConsumePublicKeyAddress
txInput2 = TxInput txOutRef2 TxConsumePublicKeyAddress
txInput3 = TxInput txOutRef3 TxConsumePublicKeyAddress
txInput4 = TxInput txOutRef4 TxConsumePublicKeyAddress
txInput5 = TxInput txOutRef5 TxConsumePublicKeyAddress

validAssetClasses :: Assertion
validAssetClasses =
  uniqueAssetClasses (map (txOutValue . snd) [utxo1, utxo2, utxo3, utxo4, utxo5])
    @?= Set.fromList
      [ Value.assetClass Ada.adaSymbol Ada.adaToken
      , Value.assetClass currencySymbol2 "Token2"
      , Value.assetClass currencySymbol1 "Token1"
      ]

testOutputValue1, testOutputValue2, testOutputValue3, testOutputValue4 :: Value
testOutputValue1 = Ada.lovelaceValueOf 1_000_000 <> Value.singleton currencySymbol1 "Token1" 100
testOutputValue2 = Ada.lovelaceValueOf 1_000_000
testOutputValue3 =
  Ada.lovelaceValueOf 1_000_000 <> Value.singleton currencySymbol1 "Token1" 100
    <> Value.singleton currencySymbol2 "Token2" 50
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
          [ [txInput2, txInput4]
          , [txInput2]
          , [txInput4, txInput5, txInput1]
          , [txInput1, txInput2]
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
      let result :: Either Text (Either WAPI.WalletAPIError (Set TxInput))
          result =
            fst $
              runPABEffectPure def $
                selectTxIns @() @'[PABEffect ()] mempty utxos (txOutValue txOutput)

          isScriptOutput :: TxOut -> Bool
          isScriptOutput (TxOut (CApi.TxOut addr _ _ _)) = not $ CApi.isKeyAddress addr

          sufficientValue :: Bool
          sufficientValue =
            txOutValue txOutput
              `leq` foldOf (folded . filtered (not . isScriptOutput) . to txOutValue) utxos

          toTxOut :: TxInput -> TxOut
          toTxOut = fromJust . (`Map.lookup` utxos) . txInputRef
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
