module Arkham.Types.Enemy.Cards.IcyGhoul
  ( icyGhoul
  , IcyGhoul(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher

newtype IcyGhoul = IcyGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

icyGhoul :: EnemyCard IcyGhoul
icyGhoul = enemyWith
  IcyGhoul
  Cards.icyGhoul
  (3, Static 4, 4)
  (2, 1)
  (spawnAtL ?~ LocationWithTitle "Cellar")

instance EnemyRunner env => RunMessage env IcyGhoul where
  runMessage msg (IcyGhoul attrs) = IcyGhoul <$> runMessage msg attrs
