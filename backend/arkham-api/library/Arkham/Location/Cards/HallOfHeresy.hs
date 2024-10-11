module Arkham.Location.Cards.HallOfHeresy (hallOfHeresy, HallOfHeresy (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HallOfHeresy = HallOfHeresy LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hallOfHeresy :: LocationCard HallOfHeresy
hallOfHeresy =
  symbolLabel $ location HallOfHeresy Cards.hallOfHeresy 4 (PerPlayer 1)

instance HasModifiersFor HallOfHeresy where
  getModifiersFor target (HallOfHeresy a) | isTarget a target = do
    hasInvestigator <- selectAny $ investigatorAt a
    toModifiers a [InVictoryDisplayForCountingVengeance | hasInvestigator]
  getModifiersFor _ _ = pure []

instance RunMessage HallOfHeresy where
  runMessage msg (HallOfHeresy attrs) = HallOfHeresy <$> runMessage msg attrs
