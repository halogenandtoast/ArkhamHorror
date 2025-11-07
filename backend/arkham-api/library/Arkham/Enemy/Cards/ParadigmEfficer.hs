module Arkham.Enemy.Cards.ParadigmEfficer (paradigmEfficer) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ParadigmEfficer = ParadigmEfficer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

paradigmEfficer :: EnemyCard ParadigmEfficer
paradigmEfficer = enemy ParadigmEfficer Cards.paradigmEfficer (3, Static 3, 3) (1, 1)

instance RunMessage ParadigmEfficer where
  runMessage msg (ParadigmEfficer attrs) = runQueueT $ case msg of
    _ -> ParadigmEfficer <$> liftRunMessage msg attrs
