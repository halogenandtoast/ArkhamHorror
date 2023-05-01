module Arkham.Location.Cards.HauntedFieldsSpectral
  ( hauntedFieldsSpectral
  , HauntedFieldsSpectral(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HauntedFieldsSpectral = HauntedFieldsSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntedFieldsSpectral :: LocationCard HauntedFieldsSpectral
hauntedFieldsSpectral = location HauntedFieldsSpectral Cards.hauntedFieldsSpectral 3 (Static 0)

instance HasAbilities HauntedFieldsSpectral where
  getAbilities (HauntedFieldsSpectral attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HauntedFieldsSpectral where
  runMessage msg (HauntedFieldsSpectral attrs) =
    HauntedFieldsSpectral <$> runMessage msg attrs
