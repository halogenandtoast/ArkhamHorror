module Arkham.Enemy.Cards.HydrasBrood
  ( hydrasBrood
  , HydrasBrood(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HydrasBrood = HydrasBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hydrasBrood :: EnemyCard HydrasBrood
hydrasBrood = enemy HydrasBrood Cards.hydrasBrood (0, Static 1, 0) (0, 0)

instance RunMessage HydrasBrood where
  runMessage msg (HydrasBrood attrs) = runQueueT $ case msg of
    _ -> HydrasBrood <$> liftRunMessage msg attrs
