module Arkham.Location.Cards.HallOfTheDeep
  ( hallOfTheDeep
  , HallOfTheDeep(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfTheDeep = HallOfTheDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfTheDeep :: LocationCard HallOfTheDeep
hallOfTheDeep = location HallOfTheDeep Cards.hallOfTheDeep 3 (PerPlayer 1)

instance HasAbilities HallOfTheDeep where
  getAbilities (HallOfTheDeep attrs) =
    extendRevealed attrs []

instance RunMessage HallOfTheDeep where
  runMessage msg (HallOfTheDeep attrs) = runQueueT $ case msg of
    _ -> HallOfTheDeep <$> liftRunMessage msg attrs
