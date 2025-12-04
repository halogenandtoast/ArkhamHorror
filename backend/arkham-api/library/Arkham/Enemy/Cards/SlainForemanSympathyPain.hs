module Arkham.Enemy.Cards.SlainForemanSympathyPain (slainForemanSympathyPain) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SlainForemanSympathyPain = SlainForemanSympathyPain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slainForemanSympathyPain :: EnemyCard SlainForemanSympathyPain
slainForemanSympathyPain = enemy SlainForemanSympathyPain Cards.slainForemanSympathyPain (0, Static 1, 0) (0, 0)

instance RunMessage SlainForemanSympathyPain where
  runMessage msg (SlainForemanSympathyPain attrs) = runQueueT $ case msg of
    _ -> SlainForemanSympathyPain <$> liftRunMessage msg attrs
