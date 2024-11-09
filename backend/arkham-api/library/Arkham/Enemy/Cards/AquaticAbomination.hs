module Arkham.Enemy.Cards.AquaticAbomination
  ( aquaticAbomination
  , AquaticAbomination(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AquaticAbomination = AquaticAbomination EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aquaticAbomination :: EnemyCard AquaticAbomination
aquaticAbomination = enemy AquaticAbomination Cards.aquaticAbomination (0, Static 1, 0) (0, 0)

instance RunMessage AquaticAbomination where
  runMessage msg (AquaticAbomination attrs) = runQueueT $ case msg of
    _ -> AquaticAbomination <$> liftRunMessage msg attrs
