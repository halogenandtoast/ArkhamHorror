module Arkham.Enemy.Cards.WolfManDrew (wolfManDrew) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfManDrew :: EnemyCard WolfManDrew
wolfManDrew = enemyWith WolfManDrew Cards.wolfManDrew (4, Static 4, 2) (2, 0) (spawnAtL ?~ "Downtown")

instance HasAbilities WolfManDrew where
  getAbilities (WolfManDrew a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)

instance RunMessage WolfManDrew where
  runMessage msg e@(WolfManDrew attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> WolfManDrew <$> liftRunMessage msg attrs
