{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.CoinSelection (valueToVec, valuesToVecs, selectTxIns, uniqueAssetClasses) where

import BotPlutusInterface.Effects (PABEffect, printBpiLog)
import BotPlutusInterface.Types (LogLevel (Debug), LogType (CoinSelectionLog))
import Control.Lens (
  foldOf,
  folded,
  ifolded,
  ix,
  over,
  to,
  uncons,
  withIndex,
  (%~),
  (&),
  (^..),
  (^?),
  _Just,
 )
import Control.Monad.Except (throwError)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, newEitherT, runEitherT)
import Data.Default (Default (def))
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
import Prettyprinter (pretty, (<+>))
import Prelude

-- 'Search' represents the possible search strategy.
data Search
  = -- | This is a greedy search that searches for nearest utxo using l2norm.
    Greedy
  | -- | This is like greedy search, but here there's
    -- additonal goal that the change utxo should be equal to the output utxo.
    GreedyApprox
  deriving stock (Eq, Show)

instance Default Search where
  def = GreedyApprox

-- 'selectTxIns' selects utxos using default search strategy, it also preprocesses
-- the utxos values in to normalized vectors. So that distances between utxos can be calculated.
selectTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Set TxIn ->
  Map TxOutRef TxOut ->
  Value ->
  Eff effs (Either Text (Set TxIn))
selectTxIns originalTxIns utxosIndex outValue =
  runEitherT $ do
    let -- This represents the input value.
        txInsValue :: Value
        txInsValue =
          foldOf (folded . to ((`Map.lookup` utxosIndex) . txInRef) . folded . to txOutValue) originalTxIns

        -- This is set of all the asset classes present in outValue, inputValue and all the utxos combined
        allAssetClasses :: Set AssetClass
        allAssetClasses =
          uniqueAssetClasses $ txInsValue : outValue : utxosIndex ^.. folded . to txOutValue

        txInRefs :: [TxOutRef]
        txInRefs = originalTxIns ^.. folded . to txInRef

        -- All the remainingUtxos that has not been used as an input to the transaction yet.
        remainingUtxos :: [(TxOutRef, TxOut)]
        remainingUtxos =
          Map.toList $
            Map.filterWithKey
              (\k v -> k `notElem` txInRefs && isRight (txOutToTxIn (k, v)))
              utxosIndex

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "Remaining UTxOs: " <+> pretty remainingUtxos

    -- the input vector for the current transaction, this can be a zero vector when there are no
    -- inputs the transaction.
    txInsVec <-
      hoistEither $
        if Value.isZero txInsValue
          then Right $ zeroVec (length allAssetClasses)
          else valueToVec allAssetClasses txInsValue

    -- the output vector of the current transaction, this is all the values of TxOut combined.
    outVec <- hoistEither $ valueToVec allAssetClasses outValue

    -- all the remainingUtxos converted to the vectors.
    remainingUtxosVec <- hoistEither $ mapM (valueToVec allAssetClasses . txOutValue . snd) remainingUtxos

    -- we use the default search strategy to get indexes of optimal utxos, these indexes are for the
    -- remainingUtxos, as we are sampling utxos from that set.
    selectedUtxosIdxs <- newEitherT $ selectTxIns' @w def (isSufficient outVec) outVec txInsVec remainingUtxosVec

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "" <+> "Selected UTxOs Index: " <+> pretty selectedUtxosIdxs

    let -- These are the selected utxos that we get using `selectedUtxosIdxs`.
        selectedUtxos :: [(TxOutRef, TxOut)]
        selectedUtxos = selectedUtxosIdxs ^.. folded . to (\idx -> remainingUtxos ^? ix idx) . folded

    selectedTxIns <- hoistEither $ mapM txOutToTxIn selectedUtxos

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "Selected TxIns: " <+> pretty selectedTxIns

    -- Now we add the selected utxos to originalTxIns present in the transaction previously.
    return $ originalTxIns <> Set.fromList selectedTxIns
  where
    -- This represents the condition when we can stop searching for utxos.
    -- First condition is that the input vector must not be zero vector, i.e.
    -- There must be atleast some input to the transaction.
    -- Second condition is that all the values of input vector must be greater than
    -- or equal to the output vector.
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
selectTxIns' searchStrategy stopSearch outVec txInsVec utxosVec
  | searchStrategy == Greedy =
    printBpiLog @w (Debug [CoinSelectionLog]) "Selecting UTxOs via greedy search"
      >> greedySearch @w stopSearch outVec txInsVec utxosVec
  | searchStrategy == GreedyApprox =
    printBpiLog @w (Debug [CoinSelectionLog]) "Selecting UTxOs via greedy pruning search"
      >> greedyApprox @w stopSearch outVec txInsVec utxosVec
  | otherwise = return $ throwError "Not a valid search strategy."

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
      utxosDist <- hoistEither $ mapM (addVec txInsVec) utxosVec >>= mapM (l2norm outVec)

      let sortedDist :: [(Int, Float)]
          sortedDist =
            utxosDist ^.. ifolded . withIndex
              & id %~ List.sortBy (\a b -> compare (snd a) (snd b))

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

greedyApprox ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  (Vector Integer -> Bool) ->
  Vector Integer ->
  Vector Integer ->
  [Vector Integer] ->
  Eff effs (Either Text [Int])
greedyApprox stopSearch outVec txInsVec utxosVec
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
      fmap (Value.assetClassValueOf v) $
        allAssetClasses & id %~ (Vec.fromList . Set.toList)

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
