module Arkham.Location.Cards.TheTwistedHollow (theTwistedHollow) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheTwistedHollow = TheTwistedHollow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedHollow :: LocationCard TheTwistedHollow
theTwistedHollow = locationWith TheTwistedHollow Cards.theTwistedHollow 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TheTwistedHollow where
  getAbilities (TheTwistedHollow a) =
    extendRevealed a []

instance RunMessage TheTwistedHollow where
  runMessage msg (TheTwistedHollow attrs) = runQueueT $ case msg of
    _ -> TheTwistedHollow <$> liftRunMessage msg attrs
