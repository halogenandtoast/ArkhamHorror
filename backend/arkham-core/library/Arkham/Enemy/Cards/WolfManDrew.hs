module Arkham.Enemy.Cards.WolfManDrew (
  WolfManDrew (..),
  wolfManDrew,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

wolfManDrew :: EnemyCard WolfManDrew
wolfManDrew = enemyWith WolfManDrew Cards.wolfManDrew (4, Static 4, 2) (2, 0) (spawnAtL ?~ "Downtown")

instance HasAbilities WolfManDrew where
  getAbilities (WolfManDrew a) =
    withBaseAbilities a
      $ [forcedAbility a 1 $ EnemyAttacks Timing.When Anyone AnyEnemyAttack (EnemyWithId $ toId a)]

instance RunMessage WolfManDrew where
  runMessage msg e@(WolfManDrew attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ HealDamage (toTarget attrs) (toAbilitySource attrs 1) 1
      pure e
    _ -> WolfManDrew <$> runMessage msg attrs
