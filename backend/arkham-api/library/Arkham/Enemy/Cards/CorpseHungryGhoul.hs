module Arkham.Enemy.Cards.CorpseHungryGhoul (corpseHungryGhoul) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CorpseHungryGhoul = CorpseHungryGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

corpseHungryGhoul :: EnemyCard CorpseHungryGhoul
corpseHungryGhoul =
  enemy CorpseHungryGhoul Cards.corpseHungryGhoul (4, Static 3, 3) (2, 2)
    & setSpawnAt "Bedroom"

instance RunMessage CorpseHungryGhoul where
  runMessage msg (CorpseHungryGhoul attrs) = CorpseHungryGhoul <$> runMessage msg attrs
