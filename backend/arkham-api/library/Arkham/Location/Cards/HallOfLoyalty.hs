module Arkham.Location.Cards.HallOfLoyalty
  ( hallOfLoyalty
  , HallOfLoyalty(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfLoyalty = HallOfLoyalty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfLoyalty :: LocationCard HallOfLoyalty
hallOfLoyalty = location HallOfLoyalty Cards.hallOfLoyalty 4 (PerPlayer 1)

instance HasAbilities HallOfLoyalty where
  getAbilities (HallOfLoyalty attrs) =
    extendRevealed attrs []

instance RunMessage HallOfLoyalty where
  runMessage msg (HallOfLoyalty attrs) = runQueueT $ case msg of
    _ -> HallOfLoyalty <$> liftRunMessage msg attrs
