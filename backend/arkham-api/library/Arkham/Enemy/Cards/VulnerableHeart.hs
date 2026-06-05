module Arkham.Enemy.Cards.VulnerableHeart (vulnerableHeart) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VulnerableHeart = VulnerableHeart EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Vulnerable Heart cannot be evaded. X is the amount of damage on it, capped at
-- 15. Modelled as a 15-health damage sponge for now (full behavior TODO).
vulnerableHeart :: EnemyCard VulnerableHeart
vulnerableHeart =
  enemyWith VulnerableHeart Cards.vulnerableHeart (-2, Static 15, 0) (2, 2)
    $ \a -> a {enemyEvade = Nothing}

instance RunMessage VulnerableHeart where
  runMessage msg (VulnerableHeart attrs) = VulnerableHeart <$> runMessage msg attrs
