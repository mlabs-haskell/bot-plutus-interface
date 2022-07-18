module BotPlutusInterface.CoinSelection (valueToVec, valuesToVecs, selectTxIns) where

import Control.Lens (Cons, cons, ix, uncons, (^?))

import Data.Either.Combinators (maybeToRight)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Ledger hiding (outValue)
import Ledger.Value qualified as Value

import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
 )

import Prelude

data Search = Greedy
  deriving stock (Show)

selectTxIns :: Set TxIn -> Map TxOutRef TxOut -> Value -> Either Text (Set TxIn)
selectTxIns originalTxIns utxosIndex outValue = do
  let txInsValue :: Value
      txInsValue =
        mconcat $ map txOutValue $ mapMaybe ((`Map.lookup` utxosIndex) . txInRef) $ Set.toList originalTxIns

      allAssetClasses :: Set AssetClass
      allAssetClasses =
        uniqueAssetClasses $ txInsValue : outValue : map (txOutValue . snd) (Map.toList utxosIndex)

      txInRefs :: [TxOutRef]
      txInRefs = map txInRef $ Set.toList originalTxIns

      remainingUtxos :: [(TxOutRef, TxOut)]
      remainingUtxos = Map.toList $ Map.filterWithKey (\k _ -> k `notElem` txInRefs) utxosIndex

  txInsVec <-
    if Value.isZero txInsValue
      then Right $ zeroVec (toInteger $ length allAssetClasses)
      else valueToVec allAssetClasses txInsValue

  outVec <- valueToVec allAssetClasses outValue

  remainingUtxosVec <- mapM (valueToVec allAssetClasses . txOutValue . snd) remainingUtxos

  selectedUtxosIdxs <- selectTxIns' Greedy (isSufficient outVec) outVec txInsVec remainingUtxosVec

  let selectedUtxos :: [(TxOutRef, TxOut)]
      selectedUtxos = mapMaybe (\idx -> remainingUtxos ^? ix (fromInteger idx)) selectedUtxosIdxs

  selectedTxIns <- mapM txOutToTxIn selectedUtxos

  return $ originalTxIns <> Set.fromList selectedTxIns
  where
    isSufficient :: Vector Integer -> Vector Integer -> Bool
    isSufficient outVec = Vec.all (== True) . Vec.zipWith (<=) outVec

selectTxIns' ::
  Search ->
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Either Text [Integer]
selectTxIns' Greedy stopSearch outVec txInsVec utxosVec =
  if null utxosVec
    then Right mempty
    else do
      utxosDist <- Vec.fromList . map (l2norm outVec) <$> mapM (addVec txInsVec) utxosVec
      let minIndex = toInteger $ Vec.minIndex utxosDist

      (selectedUtxoVec, remainingUtxosVec) <- popN utxosVec minIndex

      newTxInsVec <- addVec txInsVec selectedUtxoVec

      case stopSearch newTxInsVec of
        True -> return [minIndex]
        False -> (minIndex :) <$> selectTxIns' Greedy stopSearch outVec newTxInsVec remainingUtxosVec

l2norm :: Vector Integer -> Vector Integer -> Either Text Float
l2norm v1 v2
  | length v1 == length v2 = Right $ sqrt $ fromInteger $ sum $ Vec.zipWith formula v1 v2
  | otherwise =
    Left $
      pack $
        "Error: The length of the vectors should be same for l2norm. "
          <> "length of vector v1: "
          <> show (length v1)
          <> " "
          <> "length of vector v2: "
          <> show (length v2)
          <> "."
  where
    formula :: Integer -> Integer -> Integer
    formula n1 n2 = (n1 - n2) ^ (2 :: Integer)

addVec :: Vector Integer -> Vector Integer -> Either Text (Vector Integer)
addVec v1 v2
  | length v1 == length v2 = Right $ Vec.zipWith (+) v1 v2
  | otherwise =
    Left $
      pack $
        "Error: The length of the vectors should be same for addition."
          <> "length of vector v1: "
          <> show (length v1)
          <> " "
          <> "length of vector v2: "
          <> show (length v2)
          <> "."

zeroVec :: Integer -> Vector Integer
zeroVec n = Vec.fromList $ replicate (fromInteger n) 0

valueToVec :: Set AssetClass -> Value -> Either Text (Vector Integer)
valueToVec allAssetClasses v =
  maybeToRight "Error: Not able to uncons from empty vector." $
    fmap fst $ Vec.uncons $ valuesToVecs allAssetClasses [v]

valuesToVecs :: Set AssetClass -> [Value] -> Vector (Vector Integer)
valuesToVecs allAssetClasses values = Vec.fromList $ map toVec values
  where
    toVec :: Value -> Vector Integer
    toVec v =
      Vec.map (Value.assetClassValueOf v) $
        Vec.fromList $ Set.toList allAssetClasses

uniqueAssetClasses :: [Value] -> Set AssetClass
uniqueAssetClasses = Set.fromList . concatMap valueToAssetClass
  where
    valueToAssetClass :: Value -> [AssetClass]
    valueToAssetClass = map (\(cs, tn, _) -> Value.assetClass cs tn) . Value.flattenValue

-- Converting a chain index transaction output to a transaction input type
txOutToTxIn :: (TxOutRef, TxOut) -> Either Text TxIn
txOutToTxIn (txOutRef, txOut) =
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ -> Right $ pubKeyTxIn txOutRef
    ScriptCredential _ -> Left "Cannot covert a script output to TxIn"

popN ::
  forall (v :: Type -> Type) a.
  (Cons (v a) (v a) a a) =>
  v a ->
  Integer ->
  Either Text (a, v a)
popN va idx = do
  (a, va') <- maybeToRight "Error: Not able to uncons from empty structure." $ uncons va

  if idx == 0
    then return (a, va')
    else popN va' (idx - 1) >>= (\(a', va'') -> return (a', cons a va''))
