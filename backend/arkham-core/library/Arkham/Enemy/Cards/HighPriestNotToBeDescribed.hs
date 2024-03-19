module Arkham.Enemy.Cards.HighPriestNotToBeDescribed
  ( highPriestNotToBeDescribed
  , HighPriestNotToBeDescribed(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HighPriestNotToBeDescribed = HighPriestNotToBeDescribed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

highPriestNotToBeDescribed :: EnemyCard HighPriestNotToBeDescribed
highPriestNotToBeDescribed = enemy HighPriestNotToBeDescribed Cards.highPriestNotToBeDescribed (5, PerPlayer 3, 3) (1, 1)

instance RunMessage HighPriestNotToBeDescribed where
  runMessage msg (HighPriestNotToBeDescribed attrs) =
    HighPriestNotToBeDescribed <$> runMessage msg attrs
