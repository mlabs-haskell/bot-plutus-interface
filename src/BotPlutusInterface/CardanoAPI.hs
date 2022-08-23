module BotPlutusInterface.CardanoAPI
  (fromCardanoTxOut,
   fromCardanoTxOutDatum,
   addressInEraToAny,
   toCardanoSlotNo,
   fromCardanoSlotNo,
   fromCardanoEpochInfo,
   posixTimeToSlot
  ) where


import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF
import Cardano.Slotting.Time (SystemStart, toRelativeTime)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Bifunctor (first)
import Control.Monad.Trans.Except (runExcept)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Api qualified as CApi
import Ledger qualified
import Ledger.Tx.CardanoAPI qualified as TxApi
import Plutus.ChainIndex.Types (ChainIndexTxOut (..))
import Plutus.Contract.CardanoAPI qualified as TxApi
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Plutus.V2.Ledger.Tx qualified as V2
import PlutusTx.Prelude qualified as PlutusTx
import Prelude

fromCardanoTxOut :: CApi.TxOut CApi.CtxUTxO CApi.BabbageEra -> Either TxApi.FromCardanoError ChainIndexTxOut
fromCardanoTxOut (CApi.TxOut addr val datum refScript) =
  ChainIndexTxOut
    <$> TxApi.fromCardanoAddressInEra addr
    <*> pure (TxApi.fromCardanoValue $ CApi.txOutValueToValue val)
    <*> pure (fromCardanoTxOutDatum datum)
    <*> pure (TxApi.fromCardanoTxOutRefScript refScript)

fromCardanoTxOutDatum :: CApi.TxOutDatum CApi.CtxUTxO CApi.BabbageEra -> V2.OutputDatum
fromCardanoTxOutDatum CApi.TxOutDatumNone = V2.NoOutputDatum
fromCardanoTxOutDatum (CApi.TxOutDatumHash _ h) = V2.OutputDatumHash $ Ledger.DatumHash $ PlutusTx.toBuiltin (CApi.serialiseToRawBytes h)
fromCardanoTxOutDatum (CApi.TxOutDatumInline _ d) = V2.OutputDatum $ Ledger.Datum $ TxApi.fromCardanoScriptData d

addressInEraToAny :: CApi.AddressInEra CApi.BabbageEra -> CApi.AddressAny
addressInEraToAny (CApi.AddressInEra CApi.ByronAddressInAnyEra a) = CApi.AddressByron a
addressInEraToAny (CApi.AddressInEra (CApi.ShelleyAddressInEra _) a) = CApi.AddressShelley a

toCardanoSlotNo :: Ledger.Slot -> CApi.SlotNo
toCardanoSlotNo (Ledger.Slot s) = CApi.SlotNo $ fromInteger s

fromCardanoSlotNo :: CApi.SlotNo -> Ledger.Slot
fromCardanoSlotNo (CApi.SlotNo s) = Ledger.Slot (toInteger s)

fromCardanoEpochInfo ::
  CApi.EraHistory mode ->
  EpochInfo (Either Text)
fromCardanoEpochInfo (CApi.EraHistory _ interpreter) =
  hoistEpochInfo (first (Text.pack . show) . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

posixTimeToSlot ::
  SystemStart ->
  CApi.EraHistory CApi.CardanoMode ->
  Ledger.POSIXTime ->
  Either HF.PastHorizonException Ledger.Slot
posixTimeToSlot sysStart eraHist pTime = do
  -- toRelativeTime checks that pTime >= sysStart via `Control.Exception.assert`
  let relativeTime = toRelativeTime sysStart (toUtc pTime)
      (CApi.EraHistory _ int) = eraHist
      query = HF.wallclockToSlot relativeTime

  (sn, _, _) <- HF.interpretQuery int query
  pure (fromCardanoSlotNo sn)
  where
    toUtc :: Ledger.POSIXTime -> UTCTime
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime
        . secondsToNominalDiffTime
        $ fromInteger milliseconds / 1000
