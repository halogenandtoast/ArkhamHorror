module Arkham.Types.Enemy.Cards.WolfManDrew
  ( WolfManDrew(..)
  , wolfManDrew
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Timing qualified as Timing

newtype WolfManDrew = WolfManDrew EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolfManDrew :: EnemyCard WolfManDrew
wolfManDrew = enemyWith
  WolfManDrew
  Cards.wolfManDrew
  (4, Static 4, 2)
  (2, 0)
  (spawnAtL ?~ LocationWithTitle "Downtown")

instance HasAbilities WolfManDrew where
  getAbilities (WolfManDrew a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacks Timing.When Anyone
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env WolfManDrew where
  runMessage msg e@(WolfManDrew attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (HealDamage (toTarget attrs) 1)
    _ -> WolfManDrew <$> runMessage msg attrs
