module Arkham.Enemy.Cards.AmaranthCorruptionRevealed (amaranthCorruptionRevealed) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AmaranthCorruptionRevealed = AmaranthCorruptionRevealed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

amaranthCorruptionRevealed :: EnemyCard AmaranthCorruptionRevealed
amaranthCorruptionRevealed = enemy AmaranthCorruptionRevealed Cards.amaranthCorruptionRevealed (4, PerPlayer 6, 4) (2, 1)

instance RunMessage AmaranthCorruptionRevealed where
  runMessage msg (AmaranthCorruptionRevealed attrs) = runQueueT $ case msg of
    _ -> AmaranthCorruptionRevealed <$> liftRunMessage msg attrs
