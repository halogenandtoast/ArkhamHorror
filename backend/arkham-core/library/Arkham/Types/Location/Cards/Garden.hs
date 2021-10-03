module Arkham.Types.Location.Cards.Garden
  ( garden
  , Garden(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Garden = Garden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garden :: LocationCard Garden
garden = location Garden Cards.garden 3 (PerPlayer 1) Plus [Diamond]

instance HasAbilities Garden where
  getAbilities (Garden attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Garden where
  runMessage msg (Garden attrs) = Garden <$> runMessage msg attrs
