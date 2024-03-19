module Arkham.Enemy.Cards.LiarWithNoFace
  ( liarWithNoFace
  , LiarWithNoFace(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LiarWithNoFace = LiarWithNoFace EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

liarWithNoFace :: EnemyCard LiarWithNoFace
liarWithNoFace = enemy LiarWithNoFace Cards.liarWithNoFace (3, Static 4, 3) (0, 2)

instance RunMessage LiarWithNoFace where
  runMessage msg (LiarWithNoFace attrs) =
    LiarWithNoFace <$> runMessage msg attrs
