module Arkham.Enemy.Cards.LaboringGug (
  laboringGug,
  LaboringGug (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LaboringGug = LaboringGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

laboringGug :: EnemyCard LaboringGug
laboringGug = enemy LaboringGug Cards.laboringGug (5, Static 5, 5) (3, 1)

instance RunMessage LaboringGug where
  runMessage msg (LaboringGug attrs) =
    LaboringGug <$> runMessage msg attrs
