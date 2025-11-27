module Arkham.Location.Cards.IsolatedRoad (isolatedRoad) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IsolatedRoad = IsolatedRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isolatedRoad :: LocationCard IsolatedRoad
isolatedRoad = location IsolatedRoad Cards.isolatedRoad 3 (PerPlayer 2)

instance HasAbilities IsolatedRoad where
  getAbilities (IsolatedRoad attrs) =
    extendRevealed attrs []

instance RunMessage IsolatedRoad where
  runMessage msg (IsolatedRoad attrs) = runQueueT $ case msg of
    _ -> IsolatedRoad <$> liftRunMessage msg attrs
