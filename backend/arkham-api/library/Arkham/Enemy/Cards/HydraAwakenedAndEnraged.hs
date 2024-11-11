module Arkham.Enemy.Cards.HydraAwakenedAndEnraged (
  hydraAwakenedAndEnraged,
  HydraAwakenedAndEnraged (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HydraAwakenedAndEnraged = HydraAwakenedAndEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hydraAwakenedAndEnraged :: EnemyCard HydraAwakenedAndEnraged
hydraAwakenedAndEnraged =
  enemyWith
    HydraAwakenedAndEnraged
    Cards.hydraAwakenedAndEnraged
    (7, Static 1, 4)
    (2, 1)
    (healthL .~ Nothing)

instance RunMessage HydraAwakenedAndEnraged where
  runMessage msg (HydraAwakenedAndEnraged attrs) = runQueueT $ case msg of
    _ -> HydraAwakenedAndEnraged <$> liftRunMessage msg attrs
