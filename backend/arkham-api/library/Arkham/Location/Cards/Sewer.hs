module Arkham.Location.Cards.Sewer (sewer) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Sewer = Sewer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sewer :: LocationCard Sewer
sewer = locationWith Sewer Cards.sewer 5 (PerPlayer 1) connectsToAdjacent

instance RunMessage Sewer where
  runMessage msg (Sewer attrs) = Sewer <$> runMessage msg attrs
