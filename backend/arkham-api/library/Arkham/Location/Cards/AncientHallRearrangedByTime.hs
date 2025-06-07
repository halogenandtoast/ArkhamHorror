module Arkham.Location.Cards.AncientHallRearrangedByTime (ancientHallRearrangedByTime) where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientHallRearrangedByTime = AncientHallRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHallRearrangedByTime :: LocationCard AncientHallRearrangedByTime
ancientHallRearrangedByTime =
  location AncientHallRearrangedByTime Cards.ancientHallRearrangedByTime 3 (PerPlayer 1)
    & setLabel "ancientHall"
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities AncientHallRearrangedByTime where
  getAbilities (AncientHallRearrangedByTime attrs) =
    extendRevealed attrs []

instance RunMessage AncientHallRearrangedByTime where
  runMessage msg (AncientHallRearrangedByTime attrs) = runQueueT $ case msg of
    _ -> AncientHallRearrangedByTime <$> liftRunMessage msg attrs
