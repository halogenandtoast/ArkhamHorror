module Arkham.Location.Cards.TheGreatWebWebWovenIsland (
  theGreatWebWebWovenIsland,
  TheGreatWebWebWovenIsland (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebWebWovenIsland = TheGreatWebWebWovenIsland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebWebWovenIsland :: LocationCard TheGreatWebWebWovenIsland
theGreatWebWebWovenIsland =
  locationWith
    TheGreatWebWebWovenIsland
    Cards.theGreatWebWebWovenIsland
    1
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebWebWovenIsland where
  getAbilities (TheGreatWebWebWovenIsland attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebWebWovenIsland where
  runMessage msg (TheGreatWebWebWovenIsland attrs) = runQueueT $ case msg of
    _ -> TheGreatWebWebWovenIsland <$> lift (runMessage msg attrs)
