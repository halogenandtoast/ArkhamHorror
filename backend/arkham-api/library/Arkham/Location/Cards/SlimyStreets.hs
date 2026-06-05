module Arkham.Location.Cards.SlimyStreets (slimyStreets) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SlimyStreets = SlimyStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slimyStreets :: LocationCard SlimyStreets
slimyStreets = locationWith SlimyStreets Cards.slimyStreets 3 (PerPlayer 2) connectsToAdjacent

instance RunMessage SlimyStreets where
  runMessage msg (SlimyStreets attrs) = SlimyStreets <$> runMessage msg attrs
