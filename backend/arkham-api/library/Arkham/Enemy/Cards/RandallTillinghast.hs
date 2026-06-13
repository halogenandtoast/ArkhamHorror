module Arkham.Enemy.Cards.RandallTillinghast (randallTillinghast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype RandallTillinghast = RandallTillinghast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallTillinghast :: EnemyCard RandallTillinghast
randallTillinghast = enemy RandallTillinghast Cards.randallTillinghast (1, Static 1, 3) (1, 1)

-- TODO: abilities
instance RunMessage RandallTillinghast where
  runMessage msg (RandallTillinghast attrs) = runQueueT $ RandallTillinghast <$> liftRunMessage msg attrs
