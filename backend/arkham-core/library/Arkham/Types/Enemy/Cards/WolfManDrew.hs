module Arkham.Types.Enemy.Cards.WolfManDrew
  ( WolfManDrew(..)
  , wolfManDrew
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env WolfManDrew where
  getAbilities iid window (WolfManDrew a) =
    withBaseAbilities iid window a $ pure
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
