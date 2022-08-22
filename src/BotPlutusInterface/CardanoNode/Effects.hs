module BotPlutusInterface.CardanoNode.Effects
  ( utxosAt
  )
  where

import Data.Text qualified as Text
import Data.Text (Text)
import Ledger.Address (Address)
import Cardano.Api (LocalNodeConnectInfo(..), CardanoMode)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Ledger.Tx.CardanoAPI qualified as TxApi
import Data.Map (Map)
import Ledger.Tx (TxOutRef, TxOut)
import Cardano.Api qualified as CApi
-- import Control.Monad.Freer (Eff, Members, send, LastMember, Member, type (~>))
import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader, ask)
import Prelude

type NodeConn = LocalNodeConnectInfo CardanoMode

data QueryNode a where
  UtxosAt :: CApi.AddressAny -> QueryNode (Either Text (Map TxOutRef TxOut))

utxosAt :: forall effs. Members '[QueryNode, Reader NodeConn] effs
        => Address
        -> Eff effs (Either Text (Map TxOutRef TxOut))
utxosAt addr = runEitherT $ do
  info <- lift $ ask @NodeConn @effs

  caddr <- firstEitherT (Text.pack . show)
          $ hoistEither
          $ TxApi.toCardanoAddressInEra (localNodeNetworkId info) addr

  newEitherT $ send @QueryNode @effs (UtxosAt $ addressInEraToAny caddr)

handleQueryNode :: forall effs. (Member (Reader NodeConn) effs, LastMember IO effs)
                => Eff (QueryNode ': effs) ~> Eff effs
handleQueryNode =
  interpret $ \case
                UtxosAt addr -> handleUtxosAt addr

handleUtxosAt :: forall effs. (Member (Reader NodeConn) effs, LastMember IO effs)
              => CApi.AddressAny -> Eff effs (Either Text (Map TxOutRef TxOut))
handleUtxosAt = undefined

addressInEraToAny :: CApi.AddressInEra CApi.BabbageEra -> CApi.AddressAny
addressInEraToAny (CApi.AddressInEra CApi.ByronAddressInAnyEra    a) = CApi.AddressByron a
addressInEraToAny (CApi.AddressInEra (CApi.ShelleyAddressInEra _) a) = CApi.AddressShelley a
