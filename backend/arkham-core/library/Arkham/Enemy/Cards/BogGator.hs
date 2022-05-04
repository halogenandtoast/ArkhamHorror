module Arkham.Enemy.Cards.BogGator
  ( bogGator
  , BogGator(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
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

instance Query LocationMatcher env => HasModifiersFor env BogGator where
  getModifiersFor _ (EnemyTarget eid) (BogGator a@EnemyAttrs {..})
    | spawned a && eid == enemyId = do
      bayouLocation <- selectAny $ LocationWithTrait Bayou <> locationWithEnemy eid
      pure $ toModifiers a $ if bayouLocation
        then [EnemyFight 2, EnemyEvade 2]
        else []
  getModifiersFor _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
