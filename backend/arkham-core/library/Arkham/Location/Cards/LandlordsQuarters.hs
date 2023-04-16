module Arkham.Location.Cards.LandlordsQuarters
  ( landlordsQuarters
  , LandlordsQuarters(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LandlordsQuarters = LandlordsQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landlordsQuarters :: LocationCard LandlordsQuarters
landlordsQuarters =
  location LandlordsQuarters Cards.landlordsQuarters 2 (PerPlayer 1)

instance HasAbilities LandlordsQuarters where
  getAbilities (LandlordsQuarters attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage LandlordsQuarters where
  runMessage msg (LandlordsQuarters attrs) =
    LandlordsQuarters <$> runMessage msg attrs
