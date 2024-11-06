module Arkham.Location.Cards.HallOfBlood
  ( hallOfBlood
  , HallOfBlood(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfBlood = HallOfBlood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfBlood :: LocationCard HallOfBlood
hallOfBlood = location HallOfBlood Cards.hallOfBlood 3 (PerPlayer 1)

instance HasAbilities HallOfBlood where
  getAbilities (HallOfBlood attrs) =
    extendRevealed attrs []

instance RunMessage HallOfBlood where
  runMessage msg (HallOfBlood attrs) = runQueueT $ case msg of
    _ -> HallOfBlood <$> liftRunMessage msg attrs
