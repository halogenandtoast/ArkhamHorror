module Arkham.Types.Enemy.Cards.BogGator
  ( bogGator
  , BogGator(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

bogGator :: EnemyCard BogGator
bogGator = enemyWith
  BogGator
  Cards.bogGator
  (2, Static 2, 2)
  (1, 1)
  (preyL .~ LowestSkill SkillAgility)

instance HasSet Trait env LocationId => HasModifiersFor env BogGator where
  getModifiersFor _ (EnemyTarget eid) (BogGator a@EnemyAttrs {..})
    | spawned a && eid == enemyId = do
      bayouLocation <- member Bayou <$> getSet enemyLocation
      pure $ toModifiers a $ if bayouLocation
        then [EnemyFight 2, EnemyEvade 2]
        else []
  getModifiersFor _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
