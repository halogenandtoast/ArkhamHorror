module Arkham.Enemy.Cards.HighPriestOfHastur (highPriestOfHastur) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HighPriestOfHastur = HighPriestOfHastur EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

highPriestOfHastur :: EnemyCard HighPriestOfHastur
highPriestOfHastur = enemy HighPriestOfHastur Cards.highPriestOfHastur (6, Static 4, 2) (0, 0)

instance RunMessage HighPriestOfHastur where
  runMessage msg (HighPriestOfHastur attrs) = runQueueT $ case msg of
    _ -> HighPriestOfHastur <$> liftRunMessage msg attrs
