module BotPlutusInterface.CoinSelection (valueToVec, valuesToVecs, selectTxIn) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Vector qualified as Vec
import Data.Vector (Vector)

import Ledger hiding (outValue)
import Ledger.Value qualified as Value

import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
 )

import Prelude

data Search = Greedy
              deriving stock (Show)

selectTxIn :: Set TxIn -> Map TxOutRef TxOut -> Value -> Either Text (Set TxIn)
selectTxIn originalTxIns utxosIndex outValue = do

  let txInsValue :: Value
      txInsValue =
        mconcat $ map txOutValue $ mapMaybe ((`Map.lookup` utxosIndex) . txInRef) $ Set.toList originalTxIns

      allAssetClasses :: Set AssetClass
      allAssetClasses =
        uniqueAssetClasses $ txInsValue : outValue : map (txOutValue . snd) (Map.toList utxosIndex)

      txInRefs :: [TxOutRef]
      txInRefs = map txInRef $ Set.toList originalTxIns

      diffUtxos :: [(TxOutRef, TxOut)]
      diffUtxos = Map.toList $ Map.filterWithKey (\k _ ->  k `notElem` txInRefs) utxosIndex

      txInsVec :: Vector Integer
      txInsVec = if Value.isZero txInsValue
                   then zeroVec (toInteger $ length allAssetClasses)
                   else valueToVec allAssetClasses txInsValue

      outVec :: Vector Integer
      outVec = valueToVec allAssetClasses outValue

      diffUtxosVec :: [Vector Integer]
      diffUtxosVec = map (valueToVec allAssetClasses . txOutValue . snd) diffUtxos

  diffUtxosDist <- Vec.fromList . map (l2norm outVec) <$> mapM (addVec txInsVec) diffUtxosVec

  if null diffUtxos
    then return originalTxIns
    else flip Set.insert originalTxIns <$> txOutToTxIn (diffUtxos !! Vec.minIndex diffUtxosDist)

l2norm :: Vector Integer -> Vector Integer -> Either Text Float
l2norm v1 v2
  | length v1 == length v2 = Right $ sqrt $ fromInteger $ sum $ Vec.zipWith formula v1 v2
  | otherwise              = Left $ pack
                             $ "Error: The length of the vectors should be same for l2norm. "
                            <> "length of vector v1: " <> show (length v1) <> " "
                            <> "length of vector v2: " <> show (length v2) <> "."
  where
    formula :: Integer -> Integer -> Integer
    formula n1 n2 = (n1 - n2)^(2 :: Integer)


addVec :: Vector Integer -> Vector Integer -> Either Text (Vector Integer)
addVec v1 v2
  | length v1 == length v2 = Right $ Vec.zipWith (+) v1 v2
  | otherwise              = Left $ pack
                             $ "Error: The length of the vectors should be same for addition."
                            <> "length of vector v1: " <> show (length v1) <> " "
                            <> "length of vector v2: " <> show (length v2) <> "."

zeroVec :: Integer -> Vector Integer
zeroVec n = Vec.fromList $ replicate (fromInteger n) 0

valueToVec :: Set AssetClass -> Value -> Vector Integer
valueToVec allAssetClasses v = Vec.head $ valuesToVecs allAssetClasses [v]

valuesToVecs :: Set AssetClass -> [Value] -> Vector (Vector Integer)
valuesToVecs allAssetClasses values = Vec.fromList $ map toVec values
  where
    toVec :: Value -> Vector Integer
    toVec v = Vec.map (Value.assetClassValueOf v)
            $ Vec.fromList $ Set.toList allAssetClasses

uniqueAssetClasses :: [Value] -> Set AssetClass
uniqueAssetClasses = Set.fromList . concatMap valueToAssetClass
  where
    valueToAssetClass :: Value -> [AssetClass]
    valueToAssetClass = map (\(cs,tn,_) -> Value.assetClass cs tn) . Value.flattenValue

-- Converting a chain index transaction output to a transaction input type
txOutToTxIn :: (TxOutRef, TxOut) -> Either Text TxIn
txOutToTxIn (txOutRef, txOut) =
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ -> Right $ pubKeyTxIn txOutRef
    ScriptCredential _ -> Left "Cannot covert a script output to TxIn"
