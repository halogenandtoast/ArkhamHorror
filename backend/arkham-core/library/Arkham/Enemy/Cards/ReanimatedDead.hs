module Arkham.Enemy.Cards.ReanimatedDead (
  reanimatedDead,
  ReanimatedDead (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ReanimatedDead = ReanimatedDead EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

reanimatedDead :: EnemyCard ReanimatedDead
reanimatedDead = enemy ReanimatedDead Cards.reanimatedDead (1, Static 1, 1) (1, 0)

instance RunMessage ReanimatedDead where
  runMessage msg (ReanimatedDead attrs) = ReanimatedDead <$> runMessage msg attrs
