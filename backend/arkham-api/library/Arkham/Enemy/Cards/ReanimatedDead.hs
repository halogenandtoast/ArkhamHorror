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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

reanimatedDead :: EnemyCard ReanimatedDead
reanimatedDead = enemy ReanimatedDead Cards.reanimatedDead

instance RunMessage ReanimatedDead where
  runMessage msg (ReanimatedDead attrs) = ReanimatedDead <$> runMessage msg attrs
