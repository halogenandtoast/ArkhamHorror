module Arkham.Location.Cards.StoneBridge (stoneBridge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StoneBridge = StoneBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneBridge :: LocationCard StoneBridge
stoneBridge = locationWith StoneBridge Cards.stoneBridge 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities StoneBridge where
  getAbilities (StoneBridge attrs) =
    extendRevealed attrs []

instance RunMessage StoneBridge where
  runMessage msg (StoneBridge attrs) = runQueueT $ case msg of
    _ -> StoneBridge <$> liftRunMessage msg attrs
