module BotPlutusInterface.CardanoAPI (
  fromCardanoTxOut,
  fromCardanoTxOutDatum,
  addressInEraToAny,
  toCardanoSlotNo,
  fromCardanoSlotNo,
  fromCardanoEpochInfo,
  posixTimeToSlot,
) where

import Cardano.Api qualified as CApi
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Prelude (maybeToEither)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart, toRelativeTime)
import Control.Monad.Trans.Except (runExcept)
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ledger qualified
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Tx.CardanoAPI qualified as TxApi
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF
import Plutus.Script.Utils.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Api (Credential (..))
import Plutus.V2.Ledger.Tx qualified as V2
import PlutusTx.Prelude qualified as PlutusTx
import Relude

fromCardanoTxOut :: CApi.TxOut CApi.CtxUTxO CApi.BabbageEra -> Either TxApi.FromCardanoError ChainIndexTxOut
fromCardanoTxOut (CApi.TxOut caddr val cdatum _refScript) = do
  addr <- TxApi.fromCardanoAddressInEra caddr

  case Ledger.addressCredential addr of
    ScriptCredential valHash -> do
      dat <- maybeToEither TxApi.SimpleScriptsNotSupported $ convertOutputDatum (fromCardanoTxOutDatum cdatum)
      return $
        ScriptChainIndexTxOut
          addr
          (TxApi.fromCardanoValue $ CApi.txOutValueToValue val)
          dat
          Nothing
          (valHash, Nothing)
    PubKeyCredential _ -> do
      return $
        PublicKeyChainIndexTxOut
          addr
          (TxApi.fromCardanoValue $ CApi.txOutValueToValue val)
          (convertOutputDatum $ fromCardanoTxOutDatum cdatum)
          Nothing

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
  hoistEpochInfo (first show . runExcept) $
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

convertOutputDatum :: V2.OutputDatum -> Maybe (Ledger.DatumHash, Maybe Ledger.Datum)
convertOutputDatum = \case
  V2.NoOutputDatum -> Nothing
  V2.OutputDatumHash dh -> Just (dh, Nothing)
  V2.OutputDatum d -> Just (ScriptUtils.datumHash d, Just d)
