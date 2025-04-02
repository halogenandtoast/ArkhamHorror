module Arkham.Location.Cards.HallOfTheSunlessSea (hallOfTheSunlessSea) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfTheSunlessSea = HallOfTheSunlessSea LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfTheSunlessSea :: LocationCard HallOfTheSunlessSea
hallOfTheSunlessSea = location HallOfTheSunlessSea Cards.hallOfTheSunlessSea 3 (PerPlayer 1)

instance HasAbilities HallOfTheSunlessSea where
  getAbilities (HallOfTheSunlessSea attrs) =
    extendRevealed attrs []

instance RunMessage HallOfTheSunlessSea where
  runMessage msg (HallOfTheSunlessSea attrs) = runQueueT $ case msg of
    _ -> HallOfTheSunlessSea <$> liftRunMessage msg attrs
