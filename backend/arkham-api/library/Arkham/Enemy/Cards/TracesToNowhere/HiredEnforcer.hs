module Arkham.Enemy.Cards.TracesToNowhere.HiredEnforcer (hiredEnforcer) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HiredEnforcer = HiredEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiredEnforcer :: EnemyCard HiredEnforcer
hiredEnforcer = enemy HiredEnforcer Cards.hiredEnforcer

instance HasAbilities HiredEnforcer where
  getAbilities (HiredEnforcer a) =
    extend1 a $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 3)

instance RunMessage HiredEnforcer where
  runMessage msg e@(HiredEnforcer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure e
    _ -> HiredEnforcer <$> liftRunMessage msg attrs
