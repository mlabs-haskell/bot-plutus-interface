module BotPlutusInterface.Helpers (
  addressTxOut,
  awaitTxConfirmedUntilTime,
  isZero,
  lovelaceValueOf,
  traverseKeys,
  unsafeSerialiseAddress,
  unsafeToCardanoAddressInEra,
  unsafeValueOf,
) where

import Cardano.Api qualified as CApi
import Cardano.Api.Shelley (ReferenceScript (ReferenceScriptNone))
import Control.Lens (review)
import Control.Monad (void)
import Data.Either.Combinators (fromRight)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text, pack)
import GHC.Stack (HasCallStack)
import Ledger (POSIXTime, TxId)
import Ledger qualified
import Ledger.Tx.CardanoAPI.Internal (toCardanoAddressInEra, toCardanoValue)
import Plutus.Contract.Error (AsContractError, _OtherContractError)
import Plutus.Contract.Request (RollbackState (Unknown), awaitTxStatusChange, currentNodeClientTimeRange, waitNSlots)
import Plutus.Contract.Types (Contract, throwError)
import Plutus.V1.Ledger.Value qualified as Value
import Prelude

isZero :: CApi.Value -> Bool
isZero = all ((== 0) . snd) . CApi.valueToList

awaitTxConfirmedUntilTime :: forall (w :: Type) (s :: Row Type) (e :: Type). (AsContractError e) => TxId -> POSIXTime -> Contract w s e ()
awaitTxConfirmedUntilTime txId maxTime = do
  mTx <- awaitTxStatusChange txId
  case mTx of
    Unknown -> do
      (_, curTime) <- currentNodeClientTimeRange
      if curTime > maxTime
        then
          throwError $
            review _OtherContractError $
              pack $
                "Could not find transaction - " ++ show txId ++ " - before " ++ show maxTime
        else do
          void $ waitNSlots 20
          awaitTxConfirmedUntilTime txId maxTime
    _ -> pure ()

unsafeToCardanoAddressInEra :: HasCallStack => CApi.NetworkId -> Ledger.Address -> CApi.AddressInEra CApi.BabbageEra
unsafeToCardanoAddressInEra network = fromRight (error "Failed to convert address to addressInEra") . toCardanoAddressInEra network

unsafeSerialiseAddress :: CApi.NetworkId -> Ledger.Address -> Text
unsafeSerialiseAddress network = CApi.serialiseAddress . unsafeToCardanoAddressInEra network

lovelaceValueOf :: Integer -> CApi.Value
lovelaceValueOf amt = CApi.valueFromList [(CApi.AdaAssetId, CApi.Quantity amt)]

-- Probably unsafeSingleton, also name should define value type
unsafeValueOf :: HasCallStack => Ledger.CurrencySymbol -> Ledger.TokenName -> Integer -> CApi.Value
unsafeValueOf cs tn amt = fromRight (error "Failed to convert plutus value to cardano value") $ toCardanoValue $ Value.singleton cs tn amt

addressTxOut :: CApi.AddressInEra CApi.BabbageEra -> CApi.Value -> Ledger.TxOut
addressTxOut addr v = Ledger.TxOut $ CApi.TxOut addr (CApi.TxOutValue CApi.MultiAssetInBabbageEra v) CApi.TxOutDatumNone ReferenceScriptNone

traverseKeys ::
  forall (m :: Type -> Type) (k1 :: Type) (k2 :: Type) (a :: Type).
  ( Monad m
  , Ord k2
  ) =>
  (k1 -> m k2) ->
  Map.Map k1 a ->
  m (Map.Map k2 a)
traverseKeys f = fmap Map.fromList . traverse (\(k, v) -> (,v) <$> f k) . Map.toList
