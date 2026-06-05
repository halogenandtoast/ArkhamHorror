module Arkham.Location.Cards.TheCrater (theCrater) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCrater = TheCrater LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theCrater :: LocationCard TheCrater
theCrater = locationWith TheCrater Cards.theCrater 1 (PerPlayer 2) connectsToAdjacent

instance RunMessage TheCrater where
  runMessage msg (TheCrater attrs) = TheCrater <$> runMessage msg attrs
