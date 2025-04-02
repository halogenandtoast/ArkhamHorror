module Arkham.Location.Cards.GeothermalVent (geothermalVent) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GeothermalVent = GeothermalVent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geothermalVent :: LocationCard GeothermalVent
geothermalVent = location GeothermalVent Cards.geothermalVent 4 (PerPlayer 1)

instance HasAbilities GeothermalVent where
  getAbilities (GeothermalVent attrs) =
    extendRevealed attrs []

instance RunMessage GeothermalVent where
  runMessage msg (GeothermalVent attrs) = runQueueT $ case msg of
    _ -> GeothermalVent <$> liftRunMessage msg attrs
