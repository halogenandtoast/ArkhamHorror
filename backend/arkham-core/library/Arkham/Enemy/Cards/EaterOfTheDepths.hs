module Arkham.Enemy.Cards.EaterOfTheDepths
  ( eaterOfTheDepths
  , EaterOfTheDepths(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype EaterOfTheDepths = EaterOfTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eaterOfTheDepths :: EnemyCard EaterOfTheDepths
eaterOfTheDepths = enemyWith
  EaterOfTheDepths
  Cards.eaterOfTheDepths
  (5, Static 6, 0)
  (3, 2)
  (spawnAtL ?~ SpawnAtRandomSetAsideLocation)

instance RunMessage EaterOfTheDepths where
  runMessage msg (EaterOfTheDepths attrs) =
    EaterOfTheDepths <$> runMessage msg attrs
