module Arkham.Enemy.Cards.TakadaHirokoAeroplaneMechanic
  ( takadaHirokoAeroplaneMechanic
  , TakadaHirokoAeroplaneMechanic(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TakadaHirokoAeroplaneMechanic = TakadaHirokoAeroplaneMechanic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

takadaHirokoAeroplaneMechanic :: EnemyCard TakadaHirokoAeroplaneMechanic
takadaHirokoAeroplaneMechanic = enemy TakadaHirokoAeroplaneMechanic Cards.takadaHirokoAeroplaneMechanic (0, Static 1, 0) (0, 0)

instance RunMessage TakadaHirokoAeroplaneMechanic where
  runMessage msg (TakadaHirokoAeroplaneMechanic attrs) = runQueueT $ case msg of
    _ -> TakadaHirokoAeroplaneMechanic <$> liftRunMessage msg attrs
