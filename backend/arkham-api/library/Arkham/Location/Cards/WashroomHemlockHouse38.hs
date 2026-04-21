module Arkham.Location.Cards.WashroomHemlockHouse38 (washroomHemlockHouse38) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WashroomHemlockHouse38 = WashroomHemlockHouse38 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

washroomHemlockHouse38 :: LocationCard WashroomHemlockHouse38
washroomHemlockHouse38 = locationWith WashroomHemlockHouse38 Cards.washroomHemlockHouse38 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WashroomHemlockHouse38 where
  getAbilities (WashroomHemlockHouse38 a) =
    extendRevealed a []

instance RunMessage WashroomHemlockHouse38 where
  runMessage msg (WashroomHemlockHouse38 attrs) = runQueueT $ case msg of
    _ -> WashroomHemlockHouse38 <$> liftRunMessage msg attrs
