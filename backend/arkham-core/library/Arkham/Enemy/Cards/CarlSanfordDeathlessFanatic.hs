module Arkham.Enemy.Cards.CarlSanfordDeathlessFanatic
  ( carlSanfordDeathlessFanatic
  , CarlSanfordDeathlessFanatic(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype CarlSanfordDeathlessFanatic = CarlSanfordDeathlessFanatic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

carlSanfordDeathlessFanatic :: EnemyCard CarlSanfordDeathlessFanatic
carlSanfordDeathlessFanatic = enemy CarlSanfordDeathlessFanatic Cards.carlSanfordDeathlessFanatic (4, PerPlayer 6, 4) (1, 3)

instance RunMessage CarlSanfordDeathlessFanatic where
  runMessage msg (CarlSanfordDeathlessFanatic attrs) =
    CarlSanfordDeathlessFanatic <$> runMessage msg attrs
