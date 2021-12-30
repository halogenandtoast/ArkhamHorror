module Arkham.Enemy.Cards.GhoulPriest
  ( ghoulPriest
  , GhoulPriest(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Prey
import Arkham.SkillType

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulPriest :: EnemyCard GhoulPriest
ghoulPriest = enemyWith
  GhoulPriest
  Cards.ghoulPriest
  (4, PerPlayer 5, 4)
  (2, 2)
  (preyL .~ HighestSkill SkillCombat)

instance EnemyRunner env => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
