module Arkham.Enemy.Cards.LupineThrall (lupineThrall) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype LupineThrall = LupineThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lupineThrall :: EnemyCard LupineThrall
lupineThrall =
  enemy LupineThrall Cards.lupineThrall (4, Static 3, 4) (1, 1)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance RunMessage LupineThrall where
  runMessage msg (LupineThrall attrs) = LupineThrall <$> runMessage msg attrs
