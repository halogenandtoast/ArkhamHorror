module Arkham.Location.Cards.TheFarmhouse (theFarmhouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheFarmhouse = TheFarmhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFarmhouse :: LocationCard TheFarmhouse
theFarmhouse = symbolLabel $ locationWith TheFarmhouse Cards.theFarmhouse 0 (Static 0) connectsToAdjacent

instance HasAbilities TheFarmhouse where
  getAbilities (TheFarmhouse a) =
    extendRevealed a []

instance RunMessage TheFarmhouse where
  runMessage msg (TheFarmhouse attrs) = runQueueT $ case msg of
    _ -> TheFarmhouse <$> liftRunMessage msg attrs
