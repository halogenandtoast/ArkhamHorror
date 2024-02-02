module Arkham.Enemy.Cards.BogGator (bogGator, BogGator (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade)
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude
import Arkham.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

bogGator :: EnemyCard BogGator
bogGator =
  enemyWith BogGator Cards.bogGator (2, Static 2, 2) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility)

instance HasModifiersFor BogGator where
  getModifiersFor target (BogGator a) | spawned a && a `is` target = do
    bayouLocation <- selectAny $ LocationWithTrait Bayou <> locationWithEnemy (toId a)
    pure
      $ toModifiers a
      $ guard bayouLocation
      *> [Modifier.EnemyFight 2, Modifier.EnemyEvade 2]
  getModifiersFor _ _ = pure []

instance RunMessage BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
