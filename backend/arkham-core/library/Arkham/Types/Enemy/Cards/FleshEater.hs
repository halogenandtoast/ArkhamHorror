module Arkham.Types.Enemy.Cards.FleshEater
  ( fleshEater
  , FleshEater(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher

newtype FleshEater = FleshEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fleshEater :: EnemyCard FleshEater
fleshEater = enemyWith
  FleshEater
  Cards.fleshEater
  (4, Static 4, 1)
  (1, 2)
  (spawnAtL ?~ LocationWithTitle "Attic")

instance EnemyRunner env => RunMessage env FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
