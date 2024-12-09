module Arkham.Enemy.Cards.BogGator (bogGator, BogGator (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade)
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude
import Arkham.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bogGator :: EnemyCard BogGator
bogGator =
  enemyWith BogGator Cards.bogGator (2, Static 2, 2) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasModifiersFor BogGator where
  getModifiersFor (BogGator a) = do
    bayouLocation <- selectAny $ LocationWithTrait Bayou <> locationWithEnemy a
    modifySelfWhen a (bayouLocation && spawned a) [Modifier.EnemyFight 2, Modifier.EnemyEvade 2]

instance RunMessage BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
