module Arkham.Location.Cards.ForkedRail (forkedRail) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForkedRail = ForkedRail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkedRail :: LocationCard ForkedRail
forkedRail = location ForkedRail Cards.forkedRail 3 (PerPlayer 2)

instance HasAbilities ForkedRail where
  getAbilities (ForkedRail a) =
    extendRevealed a []

instance RunMessage ForkedRail where
  runMessage msg (ForkedRail attrs) = runQueueT $ case msg of
    _ -> ForkedRail <$> liftRunMessage msg attrs
