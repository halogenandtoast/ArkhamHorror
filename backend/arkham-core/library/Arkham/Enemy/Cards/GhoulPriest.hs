module Arkham.Enemy.Cards.GhoulPriest (
  ghoulPriest,
  GhoulPriest (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulPriest :: EnemyCard GhoulPriest
ghoulPriest =
  enemyWith
    GhoulPriest
    Cards.ghoulPriest
    (4, PerPlayer 5, 4)
    (2, 2)
    (preyL .~ Prey (InvestigatorWithHighestSkill SkillCombat))

instance RunMessage GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
