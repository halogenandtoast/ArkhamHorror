module Arkham.Asset.Assets.TakadaHirokoAeroplaneMechanic (
  takadaHirokoAeroplaneMechanic,
  TakadaHirokoAeroplaneMechanic (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TakadaHirokoAeroplaneMechanic = TakadaHirokoAeroplaneMechanic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takadaHirokoAeroplaneMechanic :: AssetCard TakadaHirokoAeroplaneMechanic
takadaHirokoAeroplaneMechanic = allyWith TakadaHirokoAeroplaneMechanic Cards.takadaHirokoAeroplaneMechanic (3, 3) noSlots

instance RunMessage TakadaHirokoAeroplaneMechanic where
  runMessage msg (TakadaHirokoAeroplaneMechanic attrs) = runQueueT $ case msg of
    _ -> TakadaHirokoAeroplaneMechanic <$> liftRunMessage msg attrs
