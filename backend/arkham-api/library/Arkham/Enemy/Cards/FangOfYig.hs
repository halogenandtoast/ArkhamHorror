module Arkham.Enemy.Cards.FangOfYig (fangOfYig) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype FangOfYig = FangOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fangOfYig :: EnemyCard FangOfYig
fangOfYig =
  enemy FangOfYig Cards.fangOfYig (3, Static 3, 3) (1, 1)
    & setPrey (HasMatchingTreachery $ treacheryIs Treacheries.poisoned)

instance HasModifiersFor FangOfYig where
  getModifiersFor (FangOfYig a) = do
    modifySelect
      a
      (investigatorEngagedWith a.id <> HasMatchingTreachery (treacheryIs Treacheries.poisoned))
      [CannotPlay AnyCard, CannotCommitCards AnyCard]

instance RunMessage FangOfYig where
  runMessage msg (FangOfYig attrs) = FangOfYig <$> runMessage msg attrs
