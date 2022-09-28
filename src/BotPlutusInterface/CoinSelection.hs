{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module BotPlutusInterface.CoinSelection (
  valueToVec,
  valuesToVecs,
  selectTxIns,
  uniqueAssetClasses,
) where

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
import Control.Monad.Except (foldM, throwError, unless)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, newEitherT, runEitherT, firstEitherT)
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
  TxIn (txInRef),
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
import qualified Wallet.API as WAPI

{-

The coin selection algorithm works as follows:

  we can think of a value with different native tokens and ada as vectors of n-dimension.

    Eg:

      Values:

        value1: [("ScriptHash1", "TokenName1", 1), ("", "", 1)]
        value2: [("ScriptHash1", "TokenName1", 2), ("ScriptHash2", "TokenName2", 2), ("", "", 2)]

      Converting Values to vectors:

        Here each column in the vector will represent the following assetClass:

                       [("ScriptHash1", "TokenName1"), ("ScriptHash2", "TokenName2"), ("", "")]

        value1-vector: [1,                             0,                              1      ]
        value2-vector: [2,                             2,                              2      ]

    Each element of the vector represents corresponding native tokens and ada, if a native token
    is not present in a value then we fill it with zero.

If we think of all the values as vector, then we can think of coin selection
and balancing problem in the following way:

  we have an output vector and many input vectors, and the goal is to get as close to the output vector
  as possible using all the combinations of input vectors.
  Now, since we think of value as vectors we can define what "close" will be,
  which is just the euclidean distance.

Now, let's see how the coin selection works by looking at an example:

  Eg: consider a Tx with two outputs and three utxos at user's wallet.

    output1 : [("scriptHash1", "TokenName1", 1), ("", "", 1)]
    output2 : [("", "", 2)]
    Fee     : [("", "", 3)]

    -- These are the utxos at user's wallet
    utxo1 : [("scriptHash1", "TokenName1", 10), ("", "", 10)]
    utxo2 : [("", "", 5)]
    utxo3 : [("scriptHash1", "TokenName1", 3), ("", "", 6)]

  First we will convert all our values into vectors::

                   [("scriptHash1", "TokenName1"), ("", "")]
    output-vector: [1,                             6] -- Just Adding output1 + output2 + Fee

    utxo1-vector: [10,                             10]
    utxo2-vector: [0,                              5]
    utxo3-vector: [3,                              6]

  Now, as stated above our goal is to get as close to output-vector as possible,
  but we also need to satisfy the following condition:

    1. Each column of the resultant vector must be greater than or equal to the corresponding
       column of the output vector.

       In this case this means that, the resultant vector [x , y] should be such that
       x >= 1 and y >= 6.

    2. The input vector must not be the zero vector.

  Currently we don't have any inputs to the Tx, hence the input vector will be zero vector:

    input-vector: [0, 0]

  Now, we can start searching for utxos that can satisfy our goal and mission:

    step 1. Add input vector to all the utxo vectors. In this case these vectors will be
            utxo1-vector, utxo2-vector, utxo3-vector.

            Result: [10, 10], [0, 5], [3, 6] (adding zero vector does not change the vector).

    step 2. calculate distances of previously added vectors with output vector.

            Result: [ 9.84 -- l2norm(output-vector, utxo1-vector)
                    , 1.41 -- l2norm(output-vector, utxo2-vector)
                    , 2.00 -- l2norm(output-vector, utxo3-vector)
                    ]

            As, we can see the distance between utxo1-vector and output-vector is very large
            which is to be expected as utxo1-vector contains lots of "value" of different AssetClass.

    step 3. sort the distances, and select the utxos with least distances
            until all the conditions are satisfied.

            Result: [ 1.41
                    , 2.00
                    , 9.84
                    ]

            Since, utxo2-vector has the least distance we will select that utxo.

            But, selecting utxo2-vector alone doesn't satisfy all our conditions,
            hence we will have to continue selecting. After utxo2-vector, the vector with
            least distance is utxo3-vector, hence we will select that vector.

            Now, the input vector is: [3, 11] (which is just addition of utxo2-vector & utxo3-vector).

            Since, this input vector statisfy all our conditions, we will select utxo2 & utxo3 as inputs
            to our Tx.

-}

-- | 'searchStrategy' represents the possible search strategy.
data SearchStrategy
  = -- | This is a greedy search that searches for nearest utxo using l2norm.
    Greedy
  | -- | This is like greedy search, but here there's
    -- additional goal that the change utxo should be equal to the output utxo.
    GreedyApprox
  deriving stock (Eq, Show)

defaultSearchStrategy :: SearchStrategy
defaultSearchStrategy = GreedyApprox

type ValueVector = Vector Integer

{- | 'selectTxIns' selects utxos using default search strategy, it also preprocesses
 the utxos values in to normalized vectors. So that distances between utxos can be calculated.
-}
selectTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  Set TxIn -> -- Inputs `TxIn` of the transaction.
  Map TxOutRef TxOut -> -- Map of utxos that can be spent
  Value -> -- total output value of the Tx.
  Eff effs (Either WAPI.WalletAPIError (Set TxIn))
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
    selectedUtxosIdxs <-
      firstEitherT WAPI.OtherError $
        newEitherT $
          searchTxIns @w
            defaultSearchStrategy
            (isSufficient outVec)
            outVec
            txInsVec
            remainingUtxosVec

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "" <+> "Selected UTxOs Index: " <+> pretty selectedUtxosIdxs

    let -- These are the selected utxos that we get using `selectedUtxosIdxs`.
        selectedUtxos :: [(TxOutRef, TxOut)]
        selectedUtxos = selectedUtxosIdxs ^.. folded . to (\idx -> remainingUtxos ^? ix idx) . folded

        selectedVectors :: [ValueVector]
        selectedVectors = selectedUtxosIdxs ^.. folded . to (\idx -> remainingUtxosVec ^? ix idx) . folded

    finalTxInputVector <- firstEitherT WAPI.OtherError $ hoistEither $ foldM addVec txInsVec selectedVectors
    unless (isSufficient outVec finalTxInputVector) $ throwError (WAPI.InsufficientFunds "Insufficient funds in the final vector.")

    selectedTxIns <- firstEitherT WAPI.OtherError $ hoistEither $ mapM txOutToTxIn selectedUtxos

    lift $ printBpiLog @w (Debug [CoinSelectionLog]) $ "Selected TxIns: " <+> pretty selectedTxIns

    -- Now we add the selected utxos to originalTxIns present in the transaction previously.
    pure $ originalTxIns <> Set.fromList selectedTxIns
  where
    -- This represents the condition when we can stop searching for utxos.
    -- First condition is that the input vector must not be zero vector, i.e.
    -- There must be at least some input to the transaction.
    -- Second condition is that all the values of input vector must be greater than
    -- or equal to the output vector.
    isSufficient :: ValueVector -> ValueVector -> Bool
    isSufficient outVec txInsVec =
      Vec.all (== True) (Vec.zipWith (<=) outVec txInsVec)
        && txInsVec /= zeroVec (length txInsVec)

-- `searchTxIns` searches for optimal utxos for a transaction as input given
-- current input vector, output vector and a list of all the remaining utxo vectors.
searchTxIns ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  SearchStrategy -> -- search strategy to use for selecting utxos
  (ValueVector -> Bool) -> -- condition on when to stop the search
  ValueVector -> -- output value vector of the Tx.
  ValueVector -> -- input value vector of the Tx.
  [ValueVector] -> -- all the value vectors of the utxos that can be spent.
  Eff effs (Either Text [Int])
searchTxIns Greedy stopSearch outVec txInsVec utxosVec =
  printBpiLog @w (Debug [CoinSelectionLog]) "Selecting UTxOs via greedy search"
    >> greedySearch @w stopSearch outVec txInsVec utxosVec
searchTxIns GreedyApprox stopSearch outVec txInsVec utxosVec =
  printBpiLog @w (Debug [CoinSelectionLog]) "Selecting UTxOs via greedy approx search"
    >> greedyApprox @w stopSearch outVec txInsVec utxosVec

-- `greedySearch` searches for utxos vectors for input to a transaction,
-- this is achieved by selecting the utxo vector that have closest euclidean distance
-- from output vector.
greedySearch ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  (ValueVector -> Bool) -> -- condition on when to stop the search
  ValueVector -> -- output value vector of the Tx.
  ValueVector -> -- input value vector of the Tx.
  [ValueVector] -> -- all the value vectors of the utxos that can be spent.
  Eff effs (Either Text [Int])
greedySearch stopSearch outVec txInsVec utxosVec
  -- we stop the search if there are no utxos vectors left, as we will not be able to
  -- select any further utxos as input to a transaction.
  | null utxosVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy: The list of remaining UTxO vectors in null."
      >> pure (Right mempty)
  -- we stop the search if the predicate `stopSearch` is true.
  | stopSearch txInsVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy: Stopping search early."
      >> pure (Right mempty)
  | otherwise =
    runEitherT $ do
      -- Here, we calculate the euclidean distance of the following vectors:
      -- l2norm(inputVec + remaining UTxO vector (U1), output vector).
      -- where U1 is just a vector from a list utxosVec.
      utxosDist <- hoistEither $ mapM (addVec txInsVec) utxosVec >>= mapM (l2norm outVec)

      -- Now, we fold the distances with their current indexes, and then
      -- sort (lowest to highest) them using the distance from output vector.
      let sortedDist :: [(Int, Float)]
          sortedDist =
            utxosDist ^.. ifolded . withIndex
              & id %~ List.sortBy (\a b -> compare (snd a) (snd b))

      newEitherT $ loop sortedDist txInsVec
  where
    loop :: [(Int, Float)] -> ValueVector -> Eff effs (Either Text [Int])
    loop [] _ = pure $ Right mempty
    loop ((idx, _) : remSortedDist) newTxInsVec =
      if stopSearch newTxInsVec -- we check if we should stop the search.
        then pure $ Right mempty
        else runEitherT $ do
          -- Get the selected utxo vector given the current idx.
          selectedUtxoVec <-
            hoistEither $ maybeToRight "Out of bounds" (utxosVec ^? ix idx)

          -- Add the selected utxo vector to the current tx input vector.
          newTxInsVec' <- hoistEither $ addVec newTxInsVec selectedUtxoVec

          lift $
            printBpiLog @w (Debug [CoinSelectionLog]) $
              "Loop Info: Stop search -> " <+> pretty (stopSearch newTxInsVec')
                <+> "Selected UTxo Idx :  "
                <+> pretty idx

          (idx :) <$> newEitherT (loop remSortedDist newTxInsVec')

-- 'greedyApprox' uses greedy search, but then it filters and add the utxo(s)
-- such that the change vector is close to the output vector.
--
-- Eg: output vector: [100]
--     input vector:  [0]
--
--     utxos vector: [50],[210],[500],[10]
--
--     so, now the greedy search will select the following utxos:
--     -- [10], [50], [210]
--
--     But, if we are using utxo vector with [210] then we don't need to
--     consume vectors like [10] and [50].
--     So, we filter such unnecessary vectors.
--
greedyApprox ::
  forall (w :: Type) (effs :: [Type -> Type]).
  Member (PABEffect w) effs =>
  (ValueVector -> Bool) -> -- condition on when to stop the search
  ValueVector -> -- output value vector of the Tx.
  ValueVector -> -- input value vector of the Tx.
  [ValueVector] -> -- all the value vectors of the utxos that can be spent.
  Eff effs (Either Text [Int])
greedyApprox stopSearch outVec txInsVec utxosVec
  -- we stop the search if there are no utxos vectors left, as we will not be able to
  -- select any further utxos as input to a transaction.
  | null utxosVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy Pruning: The list of remaining UTxO vectors in null."
      >> pure (Right mempty)
  -- we stop the search if the predicate `stopSearch` is true.
  | stopSearch txInsVec =
    printBpiLog @w (Debug [CoinSelectionLog]) "Greedy Pruning: Stopping search early."
      >> pure (Right mempty)
  | otherwise =
    runEitherT $ do
      -- Here, we get the selected indexes of utxo vectors using greedy search.
      selectedUtxosIdx <- newEitherT $ greedySearch @w stopSearch outVec txInsVec utxosVec

      let -- Reverse the order of the selected vectors
          -- The Idea here is that, the vectors that are selected at
          -- last will have greater distance from the output vector.
          -- Hence, they may contain all the values that's required for
          -- the output vector.
          revSelectedUtxosVec :: [ValueVector]
          revSelectedUtxosVec =
            List.reverse $ selectedUtxosIdx ^.. folded . to (\idx -> utxosVec ^? ix idx) . folded

          revSelectedUtxosIdx :: [Int]
          revSelectedUtxosIdx = List.reverse selectedUtxosIdx

      hoistEither $ loop txInsVec revSelectedUtxosIdx revSelectedUtxosVec
  where
    loop :: ValueVector -> [Int] -> [ValueVector] -> Either Text [Int]
    loop newTxInsVec (idx : idxs) (vec : vecs) = do
      -- Add the selected utxo vector to the current tx input vector.
      newTxInsVec' <- addVec newTxInsVec vec

      -- Get the old change vector
      changeVec <- subVec outVec newTxInsVec

      -- Get the new change vector
      changeVec' <- subVec outVec newTxInsVec'

      -- compare the distance between old change vector with output vector
      -- and new change vector with the output vector.
      case l2norm outVec changeVec' < l2norm outVec changeVec of
        -- If the distance between new change vector and output
        -- vector is smaller then we add that utxo vector
        True -> (idx :) <$> loop newTxInsVec' idxs vecs
        -- Else we check if we should stop the search here.
        False | stopSearch newTxInsVec -> Right mempty
        -- We add the current utxo vector.
        _ -> (idx :) <$> loop newTxInsVec' idxs vecs
    loop _newTxInsVec [] [] = pure mempty
    loop _newTxInsVec _idxs _vecs = Left "Lengths of indexes and list of vectors are not same."

-- calculate euclidean distance of two vectors, of same length/dimension.
l2norm :: ValueVector -> ValueVector -> Either Text Float
l2norm v1 v2
  | length v1 == length v2 = Right $ sqrt $ fromInteger $ sum $ Vec.zipWith formula v1 v2
  | otherwise =
    Left $
      pack $
        "Error: The length of the vectors should be same for l2norm."
          <> "length of vector v1: "
          <> show (length v1)
          <> " "
          <> "length of vector v2: "
          <> show (length v2)
          <> "."
  where
    formula :: Integer -> Integer -> Integer
    formula n1 n2 = (n1 - n2) ^ (2 :: Integer)

-- Add two vectors of same length.
addVec :: forall (n :: Type). Num n => Vector n -> Vector n -> Either Text (Vector n)
addVec = opVec (+)

-- Substract two vectors of same length.
subVec :: forall (n :: Type). Num n => Vector n -> Vector n -> Either Text (Vector n)
subVec = opVec (-)

-- create zero vector of specified length.
zeroVec :: Int -> Vector Integer
zeroVec n = Vec.replicate n 0

-- | Convert a value to a vector.
valueToVec :: Set AssetClass -> Value -> Either WAPI.WalletAPIError ValueVector
valueToVec allAssetClasses v =
  maybeToRight (WAPI.OtherError "Error: Not able to uncons from empty vector.") $
    (over _Just fst . uncons) $ valuesToVecs allAssetClasses [v]

-- | Convert values to a list of vectors.
valuesToVecs :: Set AssetClass -> [Value] -> Vector ValueVector
valuesToVecs allAssetClasses values = Vec.fromList $ map toVec values
  where
    toVec :: Value -> Vector Integer
    toVec v =
      fmap (Value.assetClassValueOf v) $
        allAssetClasses & id %~ (Vec.fromList . Set.toList)

-- | As the name suggests, we get a set of all the unique AssetClass from given the lists of values.
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

-- Apply a binary operation on two vectors of same length.
opVec ::
  forall (n :: Type).
  Num n =>
  (forall a. Num a => a -> a -> a) ->
  Vector n ->
  Vector n ->
  Either Text (Vector n)
opVec f v1 v2
  | length v1 == length v2 = Right $ Vec.zipWith f v1 v2
  | otherwise =
    Left $
      pack $
        "Error: The length of the vectors should be same for arithemetic operation."
          <> "length of vector v1: "
          <> show (length v1)
          <> " "
          <> "length of vector v2: "
          <> show (length v2)
          <> "."
