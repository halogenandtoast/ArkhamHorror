module Arkham.Enemy.Cards.HydraDeepInSlumber
  ( hydraDeepInSlumber
  , HydraDeepInSlumber(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HydraDeepInSlumber = HydraDeepInSlumber EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hydraDeepInSlumber :: EnemyCard HydraDeepInSlumber
hydraDeepInSlumber = enemy HydraDeepInSlumber Cards.hydraDeepInSlumber (0, Static 1, 0) (0, 0)

instance RunMessage HydraDeepInSlumber where
  runMessage msg (HydraDeepInSlumber attrs) = runQueueT $ case msg of
    _ -> HydraDeepInSlumber <$> liftRunMessage msg attrs
