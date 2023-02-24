module Arkham.Enemy.Cards.BogGator
  ( bogGator
  , BogGator(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding ( EnemyEvade )
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.SkillType
import Arkham.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bogGator :: EnemyCard BogGator
bogGator = enemyWith
  BogGator
  Cards.bogGator
  (2, Static 2, 2)
  (1, 1)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillAgility))

instance HasModifiersFor BogGator where
  getModifiersFor (EnemyTarget eid) (BogGator a@EnemyAttrs {..})
    | spawned a && eid == enemyId = do
      bayouLocation <-
        selectAny $ LocationWithTrait Bayou <> locationWithEnemy eid
      pure $ toModifiers a $ if bayouLocation
        then [Modifier.EnemyFight 2, Modifier.EnemyEvade 2]
        else []
  getModifiersFor _ _ = pure []

instance RunMessage BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
