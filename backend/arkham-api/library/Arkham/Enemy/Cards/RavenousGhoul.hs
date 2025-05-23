module Arkham.Enemy.Cards.RavenousGhoul (ravenousGhoul) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype RavenousGhoul = RavenousGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravenousGhoul :: EnemyCard RavenousGhoul
ravenousGhoul =
  enemyWith
    RavenousGhoul
    Cards.ravenousGhoul
    (3, Static 3, 3)
    (1, 1)
    (preyL .~ Prey LowestRemainingHealth)

instance RunMessage RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
