module Arkham.Location.Cards.WashroomHemlockHouse36 (washroomHemlockHouse36) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WashroomHemlockHouse36 = WashroomHemlockHouse36 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

washroomHemlockHouse36 :: LocationCard WashroomHemlockHouse36
washroomHemlockHouse36 = locationWith WashroomHemlockHouse36 Cards.washroomHemlockHouse36 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WashroomHemlockHouse36 where
  getAbilities (WashroomHemlockHouse36 a) =
    extendRevealed a []

instance RunMessage WashroomHemlockHouse36 where
  runMessage msg (WashroomHemlockHouse36 attrs) = runQueueT $ case msg of
    _ -> WashroomHemlockHouse36 <$> liftRunMessage msg attrs
