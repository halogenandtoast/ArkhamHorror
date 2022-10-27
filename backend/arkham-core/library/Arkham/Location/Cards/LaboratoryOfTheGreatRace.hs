module Arkham.Location.Cards.LaboratoryOfTheGreatRace
  ( laboratoryOfTheGreatRace
  , LaboratoryOfTheGreatRace(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LaboratoryOfTheGreatRace = LaboratoryOfTheGreatRace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laboratoryOfTheGreatRace :: LocationCard LaboratoryOfTheGreatRace
laboratoryOfTheGreatRace = location
  LaboratoryOfTheGreatRace
  Cards.laboratoryOfTheGreatRace
  2
  (PerPlayer 1)

instance HasAbilities LaboratoryOfTheGreatRace where
  getAbilities (LaboratoryOfTheGreatRace attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage LaboratoryOfTheGreatRace where
  runMessage msg (LaboratoryOfTheGreatRace attrs) =
    LaboratoryOfTheGreatRace <$> runMessage msg attrs
