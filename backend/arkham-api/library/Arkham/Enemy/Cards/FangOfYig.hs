module Arkham.Enemy.Cards.FangOfYig (
  fangOfYig,
  FangOfYig (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype FangOfYig = FangOfYig EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fangOfYig :: EnemyCard FangOfYig
fangOfYig =
  enemyWith
    FangOfYig
    Cards.fangOfYig
    (3, Static 3, 3)
    (1, 1)
    (preyL .~ Prey (HasMatchingTreachery $ treacheryIs Treacheries.poisoned))

instance HasModifiersFor FangOfYig where
  getModifiersFor (InvestigatorTarget iid) (FangOfYig a) = do
    affected <-
      iid
        <=~> ( investigatorEngagedWith (toId a)
                <> HasMatchingTreachery (treacheryIs Treacheries.poisoned)
             )
    pure
      $ toModifiers a
      $ if affected
        then [CannotPlay AnyCard, CannotCommitCards AnyCard]
        else []
  getModifiersFor _ _ = pure []

instance RunMessage FangOfYig where
  runMessage msg (FangOfYig attrs) = FangOfYig <$> runMessage msg attrs
