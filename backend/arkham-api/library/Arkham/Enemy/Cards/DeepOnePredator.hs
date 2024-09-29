module Arkham.Enemy.Cards.DeepOnePredator
  ( deepOnePredator
  , DeepOnePredator(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOnePredator = DeepOnePredator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

deepOnePredator :: EnemyCard DeepOnePredator
deepOnePredator = enemy DeepOnePredator Cards.deepOnePredator (4, Static 2, 2) (0, 1)

instance RunMessage DeepOnePredator where
  runMessage msg (DeepOnePredator attrs) = runQueueT $ case msg of
    _ -> DeepOnePredator <$> liftRunMessage msg attrs
