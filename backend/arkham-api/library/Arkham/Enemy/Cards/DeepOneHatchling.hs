module Arkham.Enemy.Cards.DeepOneHatchling
  ( deepOneHatchling
  , DeepOneHatchling(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOneHatchling = DeepOneHatchling EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

deepOneHatchling :: EnemyCard DeepOneHatchling
deepOneHatchling = enemy DeepOneHatchling Cards.deepOneHatchling (0, Static 1, 0) (0, 0)

instance RunMessage DeepOneHatchling where
  runMessage msg (DeepOneHatchling attrs) = runQueueT $ case msg of
    _ -> DeepOneHatchling <$> liftRunMessage msg attrs
