module BotPlutusInterface.Helpers (
  awaitTxConfirmedUntilTime,
  unsafeToCardanoAddressInEra,
  unsafeSerialiseAddress,
  lovelaceValueOf,
  unsafeValueOf,
  addressTxOut,
  traverseKeys,
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
import Ledger (POSIXTime, TxId)
import Ledger qualified as Ledger
import Ledger.Tx.CardanoAPI.Internal (toCardanoAddressInEra, toCardanoValue)
import Plutus.Contract.Error (AsContractError, _OtherContractError)
import Plutus.Contract.Request (RollbackState (Unknown), awaitTxStatusChange, currentNodeClientTimeRange, waitNSlots)
import Plutus.Contract.Types (Contract, throwError)
import Plutus.V1.Ledger.Value qualified as Value
import Prelude

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

unsafeToCardanoAddressInEra :: CApi.NetworkId -> Ledger.Address -> CApi.AddressInEra CApi.BabbageEra
unsafeToCardanoAddressInEra network = fromRight undefined . toCardanoAddressInEra network

unsafeSerialiseAddress :: CApi.NetworkId -> Ledger.Address -> Text
unsafeSerialiseAddress network = CApi.serialiseAddress . unsafeToCardanoAddressInEra network

lovelaceValueOf :: Integer -> CApi.Value
lovelaceValueOf amt = CApi.valueFromList [(CApi.AdaAssetId, CApi.Quantity amt)]

-- Probably unsafeSingleton, also name should define value type
unsafeValueOf :: Ledger.CurrencySymbol -> Ledger.TokenName -> Integer -> CApi.Value
unsafeValueOf cs tn amt = fromRight undefined $ toCardanoValue $ Value.singleton cs tn amt

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
traverseKeys f = fmap Map.fromList . traverse (\(k, v) -> (, v) <$> f k) . Map.toList
