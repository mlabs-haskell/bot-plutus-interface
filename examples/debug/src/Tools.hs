module Tools where

import Cardano.Api qualified as CAPI
import Data.Aeson ((.=))
import Data.Aeson qualified as JSON
import GHC.Natural (Natural)
import Ledger (Address (Address), PubKeyHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import Prelude
import Data.Text (Text)

pkhFromHash :: String -> PubKeyHash
pkhFromHash key =
  let res = JSON.fromJSON $ JSON.object ["getPubKeyHash" .= key]
   in case res of
        JSON.Success pkh -> pkh
        _ -> error "failed to parse pkh"

pkToAddr :: PubKeyHash -> Address
pkToAddr = flip Address Nothing . PubKeyCredential

-- addrToCapiAddr :: Natural -> Address -> Text
-- addrToCapiAddr nId addr =
--   let networkId = getNetId nId
--       capiAddr = toCardanoAddress networkId addr
--   in 
--     CAPI.serialiseAddress
--     . either (error . show) id
--     $ capiAddr

getNetId :: Natural -> CAPI.NetworkId
getNetId = \case
  0 -> CAPI.Mainnet
  n ->
    CAPI.Testnet
      . CAPI.NetworkMagic
      . fromInteger
      . toInteger
      $ n
