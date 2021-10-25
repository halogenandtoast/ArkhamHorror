module Arkham.Types.Enemy.Cards.LupineThrall
  ( LupineThrall(..)
  , lupineThrall
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype LupineThrall = LupineThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineThrall :: EnemyCard LupineThrall
lupineThrall = enemyWith
  LupineThrall
  Cards.lupineThrall
  (4, Static 3, 4)
  (1, 1)
  ((preyL .~ LowestSkill SkillAgility)
  . (spawnAtL ?~ FarthestLocationFromYou Anywhere)
  )

instance EnemyRunner env => RunMessage env LupineThrall where
  runMessage msg (LupineThrall attrs) = LupineThrall <$> runMessage msg attrs
