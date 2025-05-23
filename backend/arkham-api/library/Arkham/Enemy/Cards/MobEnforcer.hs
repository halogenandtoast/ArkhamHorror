module Arkham.Enemy.Cards.MobEnforcer (mobEnforcer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype MobEnforcer = MobEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobEnforcer :: EnemyCard MobEnforcer
mobEnforcer =
  enemyWith MobEnforcer Cards.mobEnforcer (4, Static 3, 3) (1, 0)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasAbilities MobEnforcer where
  getAbilities (MobEnforcer a) = extend1 a $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 4)

instance RunMessage MobEnforcer where
  runMessage msg e@(MobEnforcer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> MobEnforcer <$> liftRunMessage msg attrs
