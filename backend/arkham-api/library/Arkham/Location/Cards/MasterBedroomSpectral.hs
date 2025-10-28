module Arkham.Location.Cards.MasterBedroomSpectral (masterBedroomSpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype MasterBedroomSpectral = MasterBedroomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroomSpectral :: LocationCard MasterBedroomSpectral
masterBedroomSpectral = location MasterBedroomSpectral Cards.masterBedroomSpectral 3 (PerPlayer 1)

instance HasAbilities MasterBedroomSpectral where
  getAbilities (MasterBedroomSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "masterBedroomSpectral.haunted" a 1

instance RunMessage MasterBedroomSpectral where
  runMessage msg l@(MasterBedroomSpectral attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      placeCluesOnLocation iid (attrs.ability 1) 1
      pure l
    _ -> MasterBedroomSpectral <$> liftRunMessage msg attrs
