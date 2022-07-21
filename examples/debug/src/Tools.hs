module Tools where

import Cardano.Api qualified as CAPI
import Data.String (fromString)
import Data.Text (Text)
import GHC.Word (Word32)
import Ledger (Address (Address), PubKeyHash)
import Ledger.Tx.CardanoAPI (toCardanoAddress)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import Prelude

pkhFromHash :: String -> PubKeyHash
pkhFromHash = fromString

pkToAddr :: PubKeyHash -> Address
pkToAddr = flip Address Nothing . PubKeyCredential

addrToCapiAddr :: Word32 -> Address -> Text
addrToCapiAddr nId addr =
  let networkId = getNetId nId
      capiAddr = toCardanoAddress networkId addr
   in CAPI.serialiseAddress
        . either (error . show) id
        $ capiAddr

getNetId :: Word32 -> CAPI.NetworkId
getNetId = \case
  0 -> CAPI.Mainnet
  n ->
    CAPI.Testnet $
      CAPI.NetworkMagic n
