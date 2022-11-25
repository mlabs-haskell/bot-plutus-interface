module BotPlutusInterface.CardanoAPI (
  addressInEraToAny,
) where

import Cardano.Api qualified as CApi

addressInEraToAny :: CApi.AddressInEra CApi.BabbageEra -> CApi.AddressAny
addressInEraToAny (CApi.AddressInEra CApi.ByronAddressInAnyEra a) = CApi.AddressByron a
addressInEraToAny (CApi.AddressInEra (CApi.ShelleyAddressInEra _) a) = CApi.AddressShelley a
