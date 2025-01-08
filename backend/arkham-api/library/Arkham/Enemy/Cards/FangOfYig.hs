module Arkham.Enemy.Cards.FangOfYig (fangOfYig, FangOfYig (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype FangOfYig = FangOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fangOfYig :: EnemyCard FangOfYig
fangOfYig =
  enemyWith FangOfYig Cards.fangOfYig (3, Static 3, 3) (1, 1)
    $ preyL
    .~ Prey (HasMatchingTreachery $ treacheryIs Treacheries.poisoned)

instance HasModifiersFor FangOfYig where
  getModifiersFor (FangOfYig a) = do
    modifySelect
      a
      (investigatorEngagedWith a.id <> HasMatchingTreachery (treacheryIs Treacheries.poisoned))
      [CannotPlay AnyCard, CannotCommitCards AnyCard]

instance RunMessage FangOfYig where
  runMessage msg (FangOfYig attrs) = FangOfYig <$> runMessage msg attrs
