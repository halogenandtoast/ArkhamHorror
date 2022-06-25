module Arkham.Location.Cards.DimStreetsTheArchway
  ( dimStreetsTheArchway
  , DimStreetsTheArchway(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DimStreetsTheArchway = DimStreetsTheArchway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsTheArchway :: LocationCard DimStreetsTheArchway
dimStreetsTheArchway = location
  DimStreetsTheArchway
  Cards.dimStreetsTheArchway
  2
  (PerPlayer 1)
  Diamond
  [Square, Equals, Star]

instance HasAbilities DimStreetsTheArchway where
  getAbilities (DimStreetsTheArchway attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DimStreetsTheArchway where
  runMessage msg (DimStreetsTheArchway attrs) =
    DimStreetsTheArchway <$> runMessage msg attrs
