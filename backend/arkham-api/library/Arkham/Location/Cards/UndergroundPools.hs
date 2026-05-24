module Arkham.Location.Cards.UndergroundPools (undergroundPools) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype UndergroundPools = UndergroundPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundPools :: LocationCard UndergroundPools
undergroundPools = locationWith UndergroundPools Cards.undergroundPools 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities UndergroundPools where
  getAbilities (UndergroundPools a) =
    extendRevealed a []

instance RunMessage UndergroundPools where
  runMessage msg (UndergroundPools attrs) = runQueueT $ case msg of
    _ -> UndergroundPools <$> liftRunMessage msg attrs
