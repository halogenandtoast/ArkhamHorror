module Arkham.Enemy.Cards.UmbralHarbinger (umbralHarbinger) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UmbralHarbinger = UmbralHarbinger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

umbralHarbinger :: EnemyCard UmbralHarbinger
umbralHarbinger = enemy UmbralHarbinger Cards.umbralHarbinger (3, Static 5, 1) (1, 1)

instance RunMessage UmbralHarbinger where
  runMessage msg (UmbralHarbinger attrs) = runQueueT $ case msg of
    _ -> UmbralHarbinger <$> liftRunMessage msg attrs
