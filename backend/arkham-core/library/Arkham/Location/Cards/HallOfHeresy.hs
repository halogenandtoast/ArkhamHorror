module Arkham.Location.Cards.HallOfHeresy (
  hallOfHeresy,
  HallOfHeresy (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype HallOfHeresy = HallOfHeresy LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

hallOfHeresy :: LocationCard HallOfHeresy
hallOfHeresy =
  symbolLabel $ location HallOfHeresy Cards.hallOfHeresy 4 (PerPlayer 1)

instance HasModifiersFor HallOfHeresy where
  getModifiersFor target (HallOfHeresy a) | isTarget a target = do
    hasInvestigator <- selectAny $ InvestigatorAt $ LocationWithId $ toId a
    pure
      $ toModifiers
        a
        [InVictoryDisplayForCountingVengeance | hasInvestigator]
  getModifiersFor _ _ = pure []

instance RunMessage HallOfHeresy where
  runMessage msg (HallOfHeresy attrs) = HallOfHeresy <$> runMessage msg attrs
