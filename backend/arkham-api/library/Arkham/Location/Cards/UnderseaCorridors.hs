module Arkham.Location.Cards.UnderseaCorridors (underseaCorridors, UnderseaCorridors (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype UnderseaCorridors = UnderseaCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaCorridors :: LocationCard UnderseaCorridors
underseaCorridors =
  locationWith UnderseaCorridors Cards.underseaCorridors 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ PartiallyFlooded)

instance HasAbilities UnderseaCorridors where
  getAbilities (UnderseaCorridors attrs) =
    extendRevealed attrs []

instance RunMessage UnderseaCorridors where
  runMessage msg (UnderseaCorridors attrs) = runQueueT $ case msg of
    _ -> UnderseaCorridors <$> liftRunMessage msg attrs
