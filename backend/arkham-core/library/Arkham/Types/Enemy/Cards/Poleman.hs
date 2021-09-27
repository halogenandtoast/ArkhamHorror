module Arkham.Types.Enemy.Cards.Poleman
  ( poleman
  , Poleman(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Prey

newtype Poleman = Poleman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

poleman :: EnemyCard Poleman
poleman = enemyWith
  Poleman
  Cards.poleman
  (4, Static 4, 2)
  (1, 1)
  ((spawnAtL ?~ LocationWithTitle "Canal-side")
  . (preyL .~ HasMostMatchingAsset (AssetWithTitle "Innocent Reveler"))
  )

instance EnemyRunner env => RunMessage env Poleman where
  runMessage msg (Poleman attrs) = Poleman <$> runMessage msg attrs
