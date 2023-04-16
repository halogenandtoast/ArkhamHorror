module Arkham.Enemy.Cards.Nahab
  ( nahab
  , Nahab(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Nahab = Nahab EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nahab :: EnemyCard Nahab
nahab = enemy Nahab Cards.nahab (1, PerPlayer 1, 3) (1, 2)

instance RunMessage Nahab where
  runMessage msg (Nahab attrs) = Nahab <$> runMessage msg attrs
