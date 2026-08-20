module Arkham.Location.Cards.TheDrownedCity.TheDoomOfArkham.WesternRooftops (westernRooftops) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith, modifySelf)
import Arkham.Location.CardDefs.TheDrownedCity.TheDoomOfArkham qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Scenarios.TheDrownedCity.TheDoomOfArkhamPartII.Helpers (rooftopsReachConnecting)

newtype WesternRooftops = WesternRooftops LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

westernRooftops :: LocationCard WesternRooftops
westernRooftops =
  location WesternRooftops Cards.westernRooftops 3 (Static 1)
    & setCostToEnterUnrevealed (GroupClueCost (PerPlayer 1) Anywhere)

neighbors :: LocationMatcher
neighbors = mapOneOf LocationWithTitle ["Northside", "Miskatonic University", "St. Mary's Hospital"]

instance HasModifiersFor WesternRooftops where
  getModifiersFor (WesternRooftops a) = do
    modifySelf a [CannotBeFlooded]
    modifySelectWith a neighbors setActiveDuringSetup [ConnectedToWhen neighbors (be a)]
    whenRevealed a $ rooftopsReachConnecting a

instance RunMessage WesternRooftops where
  runMessage msg (WesternRooftops attrs) = runQueueT $ WesternRooftops <$> liftRunMessage msg attrs
