module Arkham.Enemy.Cards.MariaDeSilvaKnowsMoreThanSheLetsOn (mariaDeSilvaKnowsMoreThanSheLetsOn) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MariaDeSilvaKnowsMoreThanSheLetsOn = MariaDeSilvaKnowsMoreThanSheLetsOn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mariaDeSilvaKnowsMoreThanSheLetsOn
  :: EnemyCard MariaDeSilvaKnowsMoreThanSheLetsOn
mariaDeSilvaKnowsMoreThanSheLetsOn =
  enemy
    MariaDeSilvaKnowsMoreThanSheLetsOn
    Cards.mariaDeSilvaKnowsMoreThanSheLetsOn
    (3, Static 4, 2)
    (1, 1)

instance RunMessage MariaDeSilvaKnowsMoreThanSheLetsOn where
  runMessage msg (MariaDeSilvaKnowsMoreThanSheLetsOn attrs) =
    MariaDeSilvaKnowsMoreThanSheLetsOn <$> runMessage msg attrs
