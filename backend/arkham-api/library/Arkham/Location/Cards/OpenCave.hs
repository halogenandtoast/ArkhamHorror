module Arkham.Location.Cards.OpenCave (openCave) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OpenCave = OpenCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openCave :: LocationCard OpenCave
openCave = locationWith OpenCave Cards.openCave 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities OpenCave where
  getAbilities (OpenCave a) =
    extendRevealed a []

instance RunMessage OpenCave where
  runMessage msg (OpenCave attrs) = runQueueT $ case msg of
    _ -> OpenCave <$> liftRunMessage msg attrs
