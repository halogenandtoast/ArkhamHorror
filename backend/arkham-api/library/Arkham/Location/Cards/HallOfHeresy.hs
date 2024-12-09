module Arkham.Location.Cards.HallOfHeresy (hallOfHeresy, HallOfHeresy (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfHeresy = HallOfHeresy LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hallOfHeresy :: LocationCard HallOfHeresy
hallOfHeresy = symbolLabel $ location HallOfHeresy Cards.hallOfHeresy 4 (PerPlayer 1)

instance HasModifiersFor HallOfHeresy where
  getModifiersFor (HallOfHeresy a) = whenRevealed a do
    modifySelfWhenM a (selectAny $ investigatorAt a) [InVictoryDisplayForCountingVengeance]

instance RunMessage HallOfHeresy where
  runMessage msg (HallOfHeresy attrs) = HallOfHeresy <$> runMessage msg attrs
