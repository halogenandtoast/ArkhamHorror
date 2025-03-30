module Arkham.Location.Cards.AncientPlanetarium (ancientPlanetarium) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log

newtype AncientPlanetarium = AncientPlanetarium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientPlanetarium :: LocationCard AncientPlanetarium
ancientPlanetarium = locationWith AncientPlanetarium Cards.ancientPlanetarium 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AncientPlanetarium where
  getAbilities (AncientPlanetarium x) =
    extendRevealed1 x
      $ restricted x 1 Here
      $ actionAbilityWithCost (SpendTokenKeyCost 2 #"0")

instance RunMessage AncientPlanetarium where
  runMessage msg l@(AncientPlanetarium attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addChaosToken #"0"
      record TheTeamPhotographedTheAstronomicalCharts
      pure l
    _ -> AncientPlanetarium <$> liftRunMessage msg attrs
