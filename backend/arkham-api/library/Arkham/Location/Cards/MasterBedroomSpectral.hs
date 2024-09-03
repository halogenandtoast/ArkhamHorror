module Arkham.Location.Cards.MasterBedroomSpectral (
  masterBedroomSpectral,
  MasterBedroomSpectral (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MasterBedroomSpectral = MasterBedroomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroomSpectral :: LocationCard MasterBedroomSpectral
masterBedroomSpectral =
  location MasterBedroomSpectral Cards.masterBedroomSpectral 3 (PerPlayer 1)

instance HasAbilities MasterBedroomSpectral where
  getAbilities (MasterBedroomSpectral attrs) =
    withBaseAbilities
      attrs
      [haunted "Place 1 of your clues on Master Bedroom." attrs 1]

instance RunMessage MasterBedroomSpectral where
  runMessage msg l@(MasterBedroomSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorPlaceCluesOnLocation iid (toAbilitySource attrs 1) 1
      pure l
    _ -> MasterBedroomSpectral <$> runMessage msg attrs
