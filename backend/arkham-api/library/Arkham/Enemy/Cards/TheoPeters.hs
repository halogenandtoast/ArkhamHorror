module Arkham.Enemy.Cards.TheoPeters (theoPeters) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Message.Lifted.Placement

newtype TheoPeters = TheoPeters EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theoPeters :: EnemyCard TheoPeters
theoPeters = enemy TheoPeters Cards.theoPeters

instance HasAbilities TheoPeters where
  getAbilities (TheoPeters a) =
    extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage TheoPeters where
  runMessage msg e@(TheoPeters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Cost: discard 3 cards at random from your hand.
      randomDiscardN iid (attrs.ability 1) 3
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> TheoPeters <$> liftRunMessage msg attrs
