module Arkham.Location.Cards.BlightedGlade (blightedGlade) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BlightedGlade = BlightedGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blightedGlade :: LocationCard BlightedGlade
blightedGlade = locationWith BlightedGlade Cards.blightedGlade 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BlightedGlade where
  getAbilities (BlightedGlade a) =
    extendRevealed a []

instance RunMessage BlightedGlade where
  runMessage msg (BlightedGlade attrs) = runQueueT $ case msg of
    _ -> BlightedGlade <$> liftRunMessage msg attrs
