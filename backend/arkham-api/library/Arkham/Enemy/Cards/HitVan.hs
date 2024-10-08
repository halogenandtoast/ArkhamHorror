module Arkham.Enemy.Cards.HitVan
  ( hitVan
  , HitVan(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HitVan = HitVan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hitVan :: EnemyCard HitVan
hitVan = enemy HitVan Cards.hitVan (3, Static 5, 3) (1, 1)

instance RunMessage HitVan where
  runMessage msg (HitVan attrs) = runQueueT $ case msg of
    _ -> HitVan <$> liftRunMessage msg attrs
