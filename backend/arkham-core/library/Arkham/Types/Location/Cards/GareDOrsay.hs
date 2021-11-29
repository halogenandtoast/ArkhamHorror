module Arkham.Types.Location.Cards.GareDOrsay
  ( gareDOrsay
  , GareDOrsay(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype GareDOrsay = GareDOrsay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gareDOrsay :: LocationCard GareDOrsay
gareDOrsay = location
  GareDOrsay
  Cards.gareDOrsay
  4
  (PerPlayer 1)
  Heart
  [Diamond, Circle, Star]

instance HasAbilities GareDOrsay where
  getAbilities (GareDOrsay attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env GareDOrsay where
  runMessage msg (GareDOrsay attrs) = GareDOrsay <$> runMessage msg attrs
