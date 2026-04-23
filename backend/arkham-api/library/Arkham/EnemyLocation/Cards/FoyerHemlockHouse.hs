module Arkham.EnemyLocation.Cards.FoyerHemlockHouse where

import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted

newtype FoyerHemlockHouse = FoyerHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

foyerHemlockHouse :: EnemyLocationCard FoyerHemlockHouse
foyerHemlockHouse = enemyLocation FoyerHemlockHouse Cards.foyerHemlockHouse (2, PerPlayer 3, 3) (2, 0)
