module Arkham.Enemy.Cards.CellKeeper
  ( cellKeeper
  , CellKeeper(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype CellKeeper = CellKeeper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cellKeeper :: EnemyCard CellKeeper
cellKeeper = enemy CellKeeper Cards.cellKeeper (3, Static 3, 2) (0, 2)

instance RunMessage CellKeeper where
  runMessage msg (CellKeeper attrs) =
    CellKeeper <$> runMessage msg attrs
