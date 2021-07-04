module Arkham.Types.Enemy.Cards.FleshEater
  ( fleshEater
  , FleshEater(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.LocationMatcher

newtype FleshEater = FleshEater EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshEater :: EnemyCard FleshEater
fleshEater = enemyWith
  FleshEater
  Cards.fleshEater
  (4, Static 4, 1)
  (1, 2)
  (spawnAtL ?~ LocationWithTitle "Attic")

instance HasModifiersFor env FleshEater where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env FleshEater where
  getActions i window (FleshEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
