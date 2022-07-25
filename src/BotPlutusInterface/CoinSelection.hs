{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.CoinSelection (valueToVec, valuesToVecs, selectTxIns, uniqueAssetClasses) where

import Control.Lens (foldOf, folded, ix, over, to, uncons, (^..), (^?), _Just)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, newEitherT, runEitherT)
import Data.Either.Combinators (isRight, maybeToRight)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Ledger qualified
import Ledger.Tx (
  TxIn (..),
  TxOut (..),
  TxOutRef (..),
 )
import Ledger.Value (AssetClass, Value)
import Ledger.Value qualified as Value

import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
 )
import BotPlutusInterface.Effects (PABEffect, printBpiLog)
import BotPlutusInterface.Types (LogLevel (Debug), LogType (CoinSelectionLog))
import Prettyprinter (pretty, (<+>))
import Prelude

data Search
  = Greedy
  | GreedyPruning
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
    let txInsValue :: Value
        txInsValue =
          foldOf (folded . to ((`Map.lookup` utxosIndex) . txInRef) . folded . to txOutValue) originalTxIns

        allAssetClasses :: Set AssetClass
        allAssetClasses =
          uniqueAssetClasses $ txInsValue : outValue : utxosIndex ^.. folded . to txOutValue

        txInRefs :: [TxOutRef]
        txInRefs = originalTxIns ^.. folded . to txInRef

        remainingUtxos :: [(TxOutRef, TxOut)]
        remainingUtxos =
          Map.toList $
            Map.filterWithKey
              (\k v -> k `notElem` txInRefs && isRight (txOutToTxIn (k, v)))
              utxosIndex

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "Remaining UTxOs: " <+> pretty remainingUtxos

    txInsVec <-
      hoistEither $
        if Value.isZero txInsValue
          then Right $ zeroVec (length allAssetClasses)
          else valueToVec allAssetClasses txInsValue

    outVec <- hoistEither $ valueToVec allAssetClasses outValue

    remainingUtxosVec <- hoistEither $ mapM (valueToVec allAssetClasses . txOutValue . snd) remainingUtxos

    selectedUtxosIdxs <- newEitherT $ selectTxIns' @w GreedyPruning (isSufficient outVec) outVec txInsVec remainingUtxosVec

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "" <+> "Selected UTxOs Index: " <+> pretty selectedUtxosIdxs

    let selectedUtxos :: [(TxOutRef, TxOut)]
        selectedUtxos = selectedUtxosIdxs ^.. folded . to (\idx -> remainingUtxos ^? ix idx) . folded

    selectedTxIns <- hoistEither $ mapM txOutToTxIn selectedUtxos

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "Selected TxIns: " <+> pretty selectedTxIns

    return $ originalTxIns <> Set.fromList selectedTxIns
  where
    isSufficient :: Vector Integer -> Vector Integer -> Bool
    isSufficient outVec txInsVec =
      Vec.all (== True) (Vec.zipWith (<=) outVec txInsVec)
        && txInsVec /= zeroVec (length txInsVec)

selectTxIns' ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Search ->
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Eff effs (Either Text [Int])
selectTxIns' Greedy = greedySearch @w
selectTxIns' GreedyPruning = greedyPruning @w

greedySearch ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Eff effs (Either Text [Int])
greedySearch stopSearch outVec txInsVec utxosVec
  | null utxosVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy: The list of remanining UTxO vectors in null."
      >> return (Right mempty)
  | stopSearch txInsVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy: Stopping search early."
      >> return (Right mempty)
  | otherwise =
    runEitherT $ do
      x <- hoistEither $ mapM (addVec txInsVec) utxosVec
      utxosDist <- hoistEither $ mapM (l2norm outVec) x

      let sortedDist :: [(Int, Float)]
          sortedDist =
            List.sortBy (\a b -> compare (snd a) (snd b)) $
              zip [0 .. length utxosVec - 1] utxosDist

      newEitherT $ loop sortedDist txInsVec
  where
    loop :: [(Int, Float)] -> Vector Integer -> Eff effs (Either Text [Int])
    loop [] _ = return $ Right mempty
    loop ((idx, _) : remSortedDist) newTxInsVec =
      if stopSearch newTxInsVec
        then return $ Right mempty
        else runEitherT $ do
          selectedUtxoVec <-
            hoistEither $ maybeToRight "Out of bounds" (utxosVec ^? ix idx)
          newTxInsVec' <- hoistEither $ addVec newTxInsVec selectedUtxoVec

          lift $
            printBpiLog @w (Debug [CoinSelectionLog]) $
              "Loop Info: Stop search -> " <+> pretty (stopSearch newTxInsVec')
                <+> "Selected UTxo Idx :  "
                <+> pretty idx

          (idx :) <$> newEitherT (loop remSortedDist newTxInsVec')

greedyPruning ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Eff effs (Either Text [Int])
greedyPruning stopSearch outVec txInsVec utxosVec
  | null utxosVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy Pruning: The list of remanining UTxO vectors in null."
      >> return (Right mempty)
  | stopSearch txInsVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy Pruning: Stopping search early."
      >> return (Right mempty)
  | otherwise =
    runEitherT $ do
      selectedUtxosIdx <- newEitherT $ greedySearch @w stopSearch outVec txInsVec utxosVec

      let revSelectedUtxosVec :: [Vector Integer]
          revSelectedUtxosVec =
            List.reverse $ selectedUtxosIdx ^.. folded . to (\idx -> utxosVec ^? ix idx) . folded

          revSelectedUtxosIdx :: [Int]
          revSelectedUtxosIdx = List.reverse selectedUtxosIdx

      hoistEither $ loop txInsVec revSelectedUtxosIdx revSelectedUtxosVec
  where
    loop :: Vector Integer -> [Int] -> [Vector Integer] -> Either Text [Int]
    loop newTxInsVec (idx : idxs) (vec : vecs) = do
      newTxInsVec' <- addVec newTxInsVec vec
      changeVec <- subVec outVec newTxInsVec
      changeVec' <- subVec outVec newTxInsVec'

      case l2norm outVec changeVec' < l2norm outVec changeVec of
        True -> (idx :) <$> loop newTxInsVec' idxs vecs
        False | stopSearch newTxInsVec -> Right mempty
        False -> (idx :) <$> loop newTxInsVec' idxs vecs
    loop _newTxInsVec [] [] = pure mempty
    loop _newTxInsVec _idxs _vecs = Left "Length of idxs and list of vecs are not same."

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

addVec :: Num n => Vector n -> Vector n -> Either Text (Vector n)
addVec = opVec (+)

subVec :: Num n => Vector n -> Vector n -> Either Text (Vector n)
subVec = opVec (-)

opVec :: Num n => (forall a. Num a => a -> a -> a) -> Vector n -> Vector n -> Either Text (Vector n)
opVec f v1 v2
  | length v1 == length v2 = Right $ Vec.zipWith f v1 v2
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

zeroVec :: Int -> Vector Integer
zeroVec n = Vec.fromList $ replicate n 0

valueToVec :: Set AssetClass -> Value -> Either Text (Vector Integer)
valueToVec allAssetClasses v =
  maybeToRight "Error: Not able to uncons from empty vector." $
    (over _Just fst . uncons) $ valuesToVecs allAssetClasses [v]

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
  case Ledger.addressCredential (txOutAddress txOut) of
    PubKeyCredential _ -> Right $ Ledger.pubKeyTxIn txOutRef
    ScriptCredential _ -> Left "Cannot covert a script output to TxIn"
