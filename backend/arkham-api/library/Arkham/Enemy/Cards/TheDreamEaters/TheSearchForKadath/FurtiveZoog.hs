module Arkham.Enemy.Cards.TheDreamEaters.TheSearchForKadath.FurtiveZoog (
  furtiveZoog,
  FurtiveZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype FurtiveZoog = FurtiveZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

furtiveZoog :: EnemyCard FurtiveZoog
furtiveZoog =
  enemyWith FurtiveZoog Cards.furtiveZoog
    $ (spawnAtL ?~ SpawnEngagedWith (InvestigatorWithLowestSkill #combat $ colocatedWithMatch You))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #combat UneliminatedInvestigator))

instance RunMessage FurtiveZoog where
  runMessage msg (FurtiveZoog attrs) =
    runQueueT
      $ FurtiveZoog
      <$> liftRunMessage msg attrs
