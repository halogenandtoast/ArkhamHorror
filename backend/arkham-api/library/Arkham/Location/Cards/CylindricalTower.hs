module Arkham.Location.Cards.CylindricalTower (cylindricalTower) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype CylindricalTower = CylindricalTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cylindricalTower :: LocationCard CylindricalTower
cylindricalTower = locationWith CylindricalTower Cards.cylindricalTower 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CylindricalTower where
  getAbilities (CylindricalTower attrs) =
    extendRevealed1 attrs
      $ restricted
        attrs
        1
        (Here <> oneOf [youExist $ oneOf [can.gain.resources, can.draw.cards], exists $ OnlyInBag #frost])
      $ actionAbilityWithCost (SpendTokenKeyCost 2 #"-2")

instance RunMessage CylindricalTower where
  runMessage msg l@(CylindricalTower attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      drawCards iid (attrs.ability 1) 2
      removeChaosToken #frost
      removeChaosToken #frost
      record TheTeamStudiedTheMuralCarvings
      pure l
    _ -> CylindricalTower <$> liftRunMessage msg attrs
