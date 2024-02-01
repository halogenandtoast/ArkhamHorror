module Arkham.Enemy.Cards.FurtiveZoog (
  furtiveZoog,
  FurtiveZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype FurtiveZoog = FurtiveZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

furtiveZoog :: EnemyCard FurtiveZoog
furtiveZoog =
  enemyWith FurtiveZoog Cards.furtiveZoog (3, Static 1, 1) (1, 0)
    $ (spawnAtL ?~ SpawnEngagedWith (InvestigatorAt YourLocation <> InvestigatorWithLowestSkill #combat))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #combat))

instance RunMessage FurtiveZoog where
  runMessage msg (FurtiveZoog attrs) =
    FurtiveZoog <$> runMessage msg attrs
