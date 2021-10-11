module Arkham.Types.Enemy.Cards.DanielChesterfield
  ( danielChesterfield
  , DanielChesterfield(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype DanielChesterfield = DanielChesterfield EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

danielChesterfield :: EnemyCard DanielChesterfield
danielChesterfield = enemyWith
  DanielChesterfield
  Cards.danielChesterfield
  (3, Static 4, 3)
  (1, 1)
  (preyL .~ HighestSkill SkillCombat)

instance EnemyRunner env => RunMessage env DanielChesterfield where
  runMessage msg (DanielChesterfield attrs) =
    DanielChesterfield <$> runMessage msg attrs
