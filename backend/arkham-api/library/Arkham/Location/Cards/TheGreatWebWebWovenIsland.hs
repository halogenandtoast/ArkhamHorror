module Arkham.Location.Cards.TheGreatWebWebWovenIsland (
  theGreatWebWebWovenIsland,
  TheGreatWebWebWovenIsland (..),
)
where

import Arkham.Cost
import Arkham.Direction
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebWebWovenIsland = TheGreatWebWebWovenIsland LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebWebWovenIsland :: LocationCard TheGreatWebWebWovenIsland
theGreatWebWebWovenIsland =
  locationWith
    TheGreatWebWebWovenIsland
    Cards.theGreatWebWebWovenIsland
    1
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasModifiersFor TheGreatWebWebWovenIsland where
  getModifiersFor target (TheGreatWebWebWovenIsland attrs) | attrs `is` target = do
    pure
      $ toModifiers
        attrs
        [AdditionalCostToInvestigate $ OrCost [ActionCost 1, DoomCost (toSource attrs) (toTarget attrs) 1]]
  getModifiersFor _ _ = pure []

instance HasAbilities TheGreatWebWebWovenIsland where
  getAbilities (TheGreatWebWebWovenIsland attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebWebWovenIsland where
  runMessage msg (TheGreatWebWebWovenIsland attrs) =
    TheGreatWebWebWovenIsland <$> runMessage msg attrs
