module Arkham.Location.Cards.TemporaryHQ (temporaryHQ) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TemporaryHQ = TemporaryHQ LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

temporaryHQ :: LocationCard TemporaryHQ
temporaryHQ = locationWith TemporaryHQ Cards.temporaryHQ 2 (Static 0) connectsToAdjacent

instance RunMessage TemporaryHQ where
  runMessage msg (TemporaryHQ attrs) = TemporaryHQ <$> runMessage msg attrs
