module Arkham.Location.Cards.HuntersLodge (huntersLodge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HuntersLodge = HuntersLodge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersLodge :: LocationCard HuntersLodge
huntersLodge = location HuntersLodge Cards.huntersLodge 0 (Static 0)

instance HasAbilities HuntersLodge where
  getAbilities (HuntersLodge attrs) =
    extendRevealed attrs []

instance RunMessage HuntersLodge where
  runMessage msg (HuntersLodge attrs) = runQueueT $ case msg of
    _ -> HuntersLodge <$> liftRunMessage msg attrs
