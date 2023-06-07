module Arkham.Enemy.Cards.LodgeJailor
  ( lodgeJailor
  , LodgeJailor(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LodgeJailor = LodgeJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lodgeJailor :: EnemyCard LodgeJailor
lodgeJailor = enemy LodgeJailor Cards.lodgeJailor (2, Static 3, 3) (0, 2)

instance RunMessage LodgeJailor where
  runMessage msg (LodgeJailor attrs) =
    LodgeJailor <$> runMessage msg attrs
