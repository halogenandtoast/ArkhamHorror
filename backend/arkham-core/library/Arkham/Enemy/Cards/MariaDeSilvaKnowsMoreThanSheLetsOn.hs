module Arkham.Enemy.Cards.MariaDeSilvaKnowsMoreThanSheLetsOn (
  mariaDeSilvaKnowsMoreThanSheLetsOn,
  MariaDeSilvaKnowsMoreThanSheLetsOn (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MariaDeSilvaKnowsMoreThanSheLetsOn = MariaDeSilvaKnowsMoreThanSheLetsOn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

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
