module Arkham.Asset.Assets.DesiderioDelgadoAlvarez (desiderioDelgadoAlvarez) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DesiderioDelgadoAlvarez = DesiderioDelgadoAlvarez AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiderioDelgadoAlvarez :: AssetCard DesiderioDelgadoAlvarez
desiderioDelgadoAlvarez = allyWith DesiderioDelgadoAlvarez Cards.desiderioDelgadoAlvarez (4, 1) noSlots

instance RunMessage DesiderioDelgadoAlvarez where
  runMessage msg (DesiderioDelgadoAlvarez attrs) = runQueueT $ case msg of
    _ -> DesiderioDelgadoAlvarez <$> liftRunMessage msg attrs
