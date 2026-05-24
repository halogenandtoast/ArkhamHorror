module Arkham.Location.Cards.FungalCave (fungalCave) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FungalCave = FungalCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fungalCave :: LocationCard FungalCave
fungalCave = locationWith FungalCave Cards.fungalCave 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities FungalCave where
  getAbilities (FungalCave a) =
    extendRevealed a []

instance RunMessage FungalCave where
  runMessage msg (FungalCave attrs) = runQueueT $ case msg of
    _ -> FungalCave <$> liftRunMessage msg attrs
