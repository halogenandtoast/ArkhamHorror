module Arkham.Enemy.Cards.Poleman (
  poleman,
  Poleman (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype Poleman = Poleman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

poleman :: EnemyCard Poleman
poleman =
  enemyWith
    Poleman
    Cards.poleman
    (4, Static 4, 2)
    (1, 1)
    ( (spawnAtL ?~ SpawnLocation (LocationWithTitle "Canal-side"))
        . (preyL .~ Prey (HasMostMatchingAsset (AssetWithTitle "Innocent Reveler")))
    )

instance RunMessage Poleman where
  runMessage msg (Poleman attrs) = Poleman <$> runMessage msg attrs
