module Arkham.Enemy.Cards.InnsmouthShoggoth
  ( innsmouthShoggoth
  , InnsmouthShoggoth(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype InnsmouthShoggoth = InnsmouthShoggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

innsmouthShoggoth :: EnemyCard InnsmouthShoggoth
innsmouthShoggoth = enemy InnsmouthShoggoth Cards.innsmouthShoggoth (3, Static 4, 2) (2, 2)

instance RunMessage InnsmouthShoggoth where
  runMessage msg (InnsmouthShoggoth attrs) = runQueueT $ case msg of
    _ -> InnsmouthShoggoth <$> liftRunMessage msg attrs
