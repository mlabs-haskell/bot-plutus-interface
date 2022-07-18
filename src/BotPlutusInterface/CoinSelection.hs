{-# LANGUAGE AllowAmbiguousTypes #-}

module BotPlutusInterface.CoinSelection (valueToVec, valuesToVecs, selectTxIns) where

import Control.Lens (Cons, cons, ix, uncons, (^?))
import Control.Monad.Freer (Eff, Member)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Data.Either.Combinators (isRight, maybeToRight)
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

import BotPlutusInterface.Effects (PABEffect, printBpiLog)
import BotPlutusInterface.Types (LogLevel (Notice))

import Prettyprinter (pretty, (<+>))
import Prelude

data Search = Greedy
  deriving stock (Show)

selectTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Set TxIn ->
  Map TxOutRef TxOut ->
  Value ->
  Eff effs (Either Text (Set TxIn))
selectTxIns originalTxIns utxosIndex outValue =
  runEitherT $ do
    lift $ printBpiLog @w Notice $ pretty (Map.toList utxosIndex)

    let txInsValue :: Value
        txInsValue =
          mconcat $ map txOutValue $ mapMaybe ((`Map.lookup` utxosIndex) . txInRef) $ Set.toList originalTxIns

        allAssetClasses :: Set AssetClass
        allAssetClasses =
          uniqueAssetClasses $ txInsValue : outValue : map (txOutValue . snd) (Map.toList utxosIndex)

        txInRefs :: [TxOutRef]
        txInRefs = map txInRef $ Set.toList originalTxIns

        remainingUtxos :: [(TxOutRef, TxOut)]
        remainingUtxos =
          Map.toList $
            Map.filterWithKey
              (\k v -> k `notElem` txInRefs && isRight (txOutToTxIn (k, v)))
              utxosIndex

    lift $ printBpiLog @w Notice $ "\n\n Remaining UTxOs: " <+> pretty remainingUtxos <+> "\n\n"

    txInsVec <-
      hoistEither $
        if Value.isZero txInsValue
          then Right $ zeroVec (toInteger $ length allAssetClasses)
          else valueToVec allAssetClasses txInsValue

    outVec <- hoistEither $ valueToVec allAssetClasses outValue

    lift $ printBpiLog @w Notice $ "IsSufficient: " <+> pretty (isSufficient outVec txInsVec) <+> "\n\n"

    remainingUtxosVec <- hoistEither $ mapM (valueToVec allAssetClasses . txOutValue . snd) remainingUtxos

    selectedUtxosIdxs <- hoistEither $ selectTxIns' Greedy (isSufficient outVec) outVec txInsVec remainingUtxosVec

    lift $ printBpiLog @w Notice $ "\n\n" <+> "Selected UTxOs Index: " <+> pretty selectedUtxosIdxs <+> "\n\n"

    let selectedUtxos :: [(TxOutRef, TxOut)]
        selectedUtxos = mapMaybe (\idx -> remainingUtxos ^? ix (fromInteger idx)) selectedUtxosIdxs

    selectedTxIns <- hoistEither $ mapM txOutToTxIn selectedUtxos

    lift $ printBpiLog @w Notice $ "Selected TxIns: " <+> pretty selectedTxIns <+> "\n\n"

    return $ originalTxIns <> Set.fromList selectedTxIns
  where
    isSufficient :: Vector Integer -> Vector Integer -> Bool
    isSufficient outVec txInsVec = Vec.all (== True) (Vec.zipWith (<=) outVec txInsVec)
                                && txInsVec /= zeroVec (toInteger $ length txInsVec)

selectTxIns' ::
  Search ->
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Either Text [Integer]
selectTxIns' Greedy stopSearch outVec txInsVec utxosVec
  | null utxosVec || stopSearch txInsVec = Right mempty
  | otherwise =
    do
      utxosDist <- Vec.fromList . map (l2norm outVec) <$> mapM (addVec txInsVec) utxosVec
      let minIndex = toInteger $ Vec.minIndex utxosDist

      (selectedUtxoVec, remainingUtxosVec) <- pop utxosVec minIndex

      newTxInsVec <- addVec txInsVec selectedUtxoVec

      if stopSearch newTxInsVec
        then return [minIndex]
        else (minIndex :) <$> selectTxIns' Greedy stopSearch outVec newTxInsVec remainingUtxosVec

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

pop ::
  forall (v :: Type -> Type) a.
  (Cons (v a) (v a) a a) =>
  v a ->
  Integer ->
  Either Text (a, v a)
pop va idx = do
  (a, va') <- maybeToRight "Error: Not able to uncons from empty structure." $ uncons va

  if idx == 0
    then return (a, va')
    else pop va' (idx - 1) >>= (\(a', va'') -> return (a', cons a va''))
