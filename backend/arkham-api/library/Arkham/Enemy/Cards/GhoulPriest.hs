module Arkham.Enemy.Cards.GhoulPriest (ghoulPriest) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype GhoulPriest = GhoulPriest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulPriest :: EnemyCard GhoulPriest
ghoulPriest =
  enemy GhoulPriest Cards.ghoulPriest (4, PerPlayer 5, 4) (2, 2)
    & setPrey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator)

instance RunMessage GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
