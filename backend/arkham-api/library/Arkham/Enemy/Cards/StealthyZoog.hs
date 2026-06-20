module Arkham.Enemy.Cards.StealthyZoog (
  stealthyZoog,
  StealthyZoog (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype StealthyZoog = StealthyZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stealthyZoog :: EnemyCard StealthyZoog
stealthyZoog =
  enemyWith StealthyZoog Cards.stealthyZoog
    $ (spawnAtL ?~ SpawnEngagedWith (InvestigatorWithLowestSkill #agility $ colocatedWithMatch You))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator))

instance RunMessage StealthyZoog where
  runMessage msg (StealthyZoog attrs) =
    StealthyZoog <$> runMessage msg attrs
