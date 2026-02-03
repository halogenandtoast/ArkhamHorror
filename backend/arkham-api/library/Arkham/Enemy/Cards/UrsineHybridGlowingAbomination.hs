module Arkham.Enemy.Cards.UrsineHybridGlowingAbomination (ursineHybridGlowingAbomination) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UrsineHybridGlowingAbomination = UrsineHybridGlowingAbomination EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ursineHybridGlowingAbomination :: EnemyCard UrsineHybridGlowingAbomination
ursineHybridGlowingAbomination = enemy UrsineHybridGlowingAbomination Cards.ursineHybridGlowingAbomination (5, Static 5, 3) (3, 2)

instance RunMessage UrsineHybridGlowingAbomination where
  runMessage msg (UrsineHybridGlowingAbomination attrs) = runQueueT $ case msg of
    _ -> UrsineHybridGlowingAbomination <$> liftRunMessage msg attrs
