module Arkham.Enemy.Cards.DrMalaSinhaDaringPhysician
  ( drMalaSinhaDaringPhysician
  , DrMalaSinhaDaringPhysician(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DrMalaSinhaDaringPhysician = DrMalaSinhaDaringPhysician EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

drMalaSinhaDaringPhysician :: EnemyCard DrMalaSinhaDaringPhysician
drMalaSinhaDaringPhysician = enemy DrMalaSinhaDaringPhysician Cards.drMalaSinhaDaringPhysician (0, Static 1, 0) (0, 0)

instance RunMessage DrMalaSinhaDaringPhysician where
  runMessage msg (DrMalaSinhaDaringPhysician attrs) = runQueueT $ case msg of
    _ -> DrMalaSinhaDaringPhysician <$> liftRunMessage msg attrs
