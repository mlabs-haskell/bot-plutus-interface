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
      
      -- txInsVec :: Vector Integer

  txInsVec <- valueToVec allAssetClasses txInsValue
  
  outVec <- valueToVec allAssetClasses outValue

  diffUtxosVec <- mapM (valueToVec allAssetClasses . txOutValue . snd) diffUtxos

  let diffUtxosDist :: Vector Float
      diffUtxosDist = Vec.fromList $ map (l2norm outVec . addVec txInsVec) diffUtxosVec

  txIn <- txOutToTxIn (diffUtxos !! Vec.minIndex diffUtxosDist)

  return (Set.insert txIn originalTxIns)
  
  where
    
    txInRefs :: [TxOutRef]
    txInRefs = map txInRef $ Set.toList originalTxIns

    diffUtxos :: [(TxOutRef, TxOut)]
    diffUtxos = Map.toList $ Map.filterWithKey (\k _ ->  k `notElem` txInRefs) utxosIndex

l2norm :: Vector Integer -> Vector Integer -> Float
l2norm v1 v2 = sqrt $ fromInteger $ sum $ Vec.zipWith (\n1 n2 -> (n1 - n2)^2) v1 v2

addVec :: Vector Integer -> Vector Integer -> Vector Integer
addVec v1 v2
  | length v1 == length v2 = Vec.zipWith (+) v1 v2
  | otherwise              = error "Vector lengths are not same for addition."

-- zeroVec :: Integer -> Vector Integer
-- zeroVec n = Vec.fromList $ replicate (fromInteger n) 0

valueToVec :: Set AssetClass -> Value -> Either Text (Vector Integer)
valueToVec allAssetClasses v
  | not (null allAssetClasses) && not (Value.isZero v) = Right $ Vec.head $ valuesToVecs allAssetClasses [v]
  | otherwise = Left $ pack $"Both set of AssetClass and Value should not be null. Set of AssetClass: "
             <> show allAssetClasses
             <> "value: "
             <> show v

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
