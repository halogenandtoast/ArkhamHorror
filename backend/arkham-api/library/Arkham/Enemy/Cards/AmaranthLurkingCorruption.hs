module Arkham.Enemy.Cards.AmaranthLurkingCorruption (amaranthLurkingCorruption) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AmaranthLurkingCorruption = AmaranthLurkingCorruption EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

amaranthLurkingCorruption :: EnemyCard AmaranthLurkingCorruption
amaranthLurkingCorruption = enemy AmaranthLurkingCorruption Cards.amaranthLurkingCorruption (3, PerPlayer 3, 4) (1, 2)

instance RunMessage AmaranthLurkingCorruption where
  runMessage msg (AmaranthLurkingCorruption attrs) = runQueueT $ case msg of
    _ -> AmaranthLurkingCorruption <$> liftRunMessage msg attrs
