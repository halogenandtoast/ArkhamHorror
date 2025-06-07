module Arkham.Location.Cards.GrandChamberRearrangedByTime (grandChamberRearrangedByTime) where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandChamberRearrangedByTime = GrandChamberRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamberRearrangedByTime :: LocationCard GrandChamberRearrangedByTime
grandChamberRearrangedByTime =
  location GrandChamberRearrangedByTime Cards.grandChamberRearrangedByTime 2 (PerPlayer 1)
    & setLabel "grandChamber"
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities GrandChamberRearrangedByTime where
  getAbilities (GrandChamberRearrangedByTime attrs) =
    extendRevealed attrs []

instance RunMessage GrandChamberRearrangedByTime where
  runMessage msg (GrandChamberRearrangedByTime attrs) = runQueueT $ case msg of
    _ -> GrandChamberRearrangedByTime <$> liftRunMessage msg attrs
