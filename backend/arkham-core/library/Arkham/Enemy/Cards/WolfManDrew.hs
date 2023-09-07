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
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfManDrew :: EnemyCard WolfManDrew
wolfManDrew =
  enemyWith
    WolfManDrew
    Cards.wolfManDrew
    (4, Static 4, 2)
    (2, 0)
    (spawnAtL ?~ "Downtown")

instance HasAbilities WolfManDrew where
  getAbilities (WolfManDrew a) =
    withBaseAbilities a
      $ [forcedAbility a 1 $ EnemyAttacks Timing.When Anyone AnyEnemyAttack (toEnemyMatcher $ toId a)]

instance RunMessage WolfManDrew where
  runMessage msg e@(WolfManDrew attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ HealDamage (toTarget attrs) (toSource attrs) 1
      pure e
    _ -> WolfManDrew <$> runMessage msg attrs
