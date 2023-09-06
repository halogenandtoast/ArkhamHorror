module Arkham.Enemy.Cards.LodgeEnforcer
  ( lodgeEnforcer
  , LodgeEnforcer(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LodgeEnforcer = LodgeEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lodgeEnforcer :: EnemyCard LodgeEnforcer
lodgeEnforcer = enemy LodgeEnforcer Cards.lodgeEnforcer (3, Static 4, 3) (1, 1)

instance RunMessage LodgeEnforcer where
  runMessage msg (LodgeEnforcer attrs) =
    LodgeEnforcer <$> runMessage msg attrs
