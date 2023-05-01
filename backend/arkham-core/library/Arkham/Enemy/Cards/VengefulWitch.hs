module Arkham.Enemy.Cards.VengefulWitch (
  vengefulWitch,
  VengefulWitch (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype VengefulWitch = VengefulWitch EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vengefulWitch :: EnemyCard VengefulWitch
vengefulWitch =
  enemyWith
    VengefulWitch
    Cards.vengefulWitch
    (3, Static 3, 3)
    (1, 1)
    ( spawnAtL
        ?~ SpawnLocation
          (LocationMatchAny [LocationWithTitle "The Gallows", LocationWithTitle "Heretics' Graves"])
    )

instance RunMessage VengefulWitch where
  runMessage msg (VengefulWitch attrs) =
    VengefulWitch <$> runMessage msg attrs
