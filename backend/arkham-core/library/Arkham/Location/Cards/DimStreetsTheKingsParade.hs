module Arkham.Location.Cards.DimStreetsTheKingsParade
  ( dimStreetsTheKingsParade
  , DimStreetsTheKingsParade(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DimStreetsTheKingsParade = DimStreetsTheKingsParade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsTheKingsParade :: LocationCard DimStreetsTheKingsParade
dimStreetsTheKingsParade = location
  DimStreetsTheKingsParade
  Cards.dimStreetsTheKingsParade
  2
  (PerPlayer 1)
  Diamond
  [Square, Equals, Star]

instance HasAbilities DimStreetsTheKingsParade where
  getAbilities (DimStreetsTheKingsParade attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DimStreetsTheKingsParade where
  runMessage msg (DimStreetsTheKingsParade attrs) =
    DimStreetsTheKingsParade <$> runMessage msg attrs
