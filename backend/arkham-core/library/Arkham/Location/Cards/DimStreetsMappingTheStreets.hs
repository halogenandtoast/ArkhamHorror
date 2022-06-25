module Arkham.Location.Cards.DimStreetsMappingTheStreets
  ( dimStreetsMappingTheStreets
  , DimStreetsMappingTheStreets(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DimStreetsMappingTheStreets = DimStreetsMappingTheStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsMappingTheStreets :: LocationCard DimStreetsMappingTheStreets
dimStreetsMappingTheStreets = location
  DimStreetsMappingTheStreets
  Cards.dimStreetsMappingTheStreets
  2
  (PerPlayer 1)
  Diamond
  [Square, Equals, Star]

instance HasAbilities DimStreetsMappingTheStreets where
  getAbilities (DimStreetsMappingTheStreets attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DimStreetsMappingTheStreets where
  runMessage msg (DimStreetsMappingTheStreets attrs) =
    DimStreetsMappingTheStreets <$> runMessage msg attrs
