module Arkham.Enemy.Cards.SquamousParasite (squamousParasite) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SquamousParasite = SquamousParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

squamousParasite :: EnemyCard SquamousParasite
squamousParasite = enemy SquamousParasite Cards.squamousParasite (3, Static 1, 3) (1, 0)

-- TODO: abilities
instance RunMessage SquamousParasite where
  runMessage msg (SquamousParasite attrs) = runQueueT $ SquamousParasite <$> liftRunMessage msg attrs
