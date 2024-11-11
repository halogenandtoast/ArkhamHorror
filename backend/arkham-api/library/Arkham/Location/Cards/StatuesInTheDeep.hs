module Arkham.Location.Cards.StatuesInTheDeep (statuesInTheDeep, StatuesInTheDeep (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype StatuesInTheDeep = StatuesInTheDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

statuesInTheDeep :: LocationCard StatuesInTheDeep
statuesInTheDeep =
  locationWith StatuesInTheDeep Cards.statuesInTheDeep 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ FullyFlooded)

instance HasAbilities StatuesInTheDeep where
  getAbilities (StatuesInTheDeep attrs) =
    extendRevealed attrs []

instance RunMessage StatuesInTheDeep where
  runMessage msg (StatuesInTheDeep attrs) = runQueueT $ case msg of
    _ -> StatuesInTheDeep <$> liftRunMessage msg attrs
