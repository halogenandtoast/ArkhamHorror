module Arkham.EnemyLocation.Cards.FoyerHemlockHouse where

import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted

newtype FoyerHemlockHouse = FoyerHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

foyerHemlockHouse :: EnemyLocationCard FoyerHemlockHouse
foyerHemlockHouse =
  enemyLocationWith FoyerHemlockHouse Cards.foyerHemlockHouse (2, Static 3, 3) (1, 1)
    $ baseL
    %~ \la -> la {locationShroud = Just (Static 2)}

-- "Forced - When this enemy-location is revealed: It attacks each investigator
-- at this location."
-- Routed via the scenario when FlipToEnemyLocation fires; the scenario pushes
-- `Do EnemiesAttack` against this enemy-location's id.
instance RunMessage FoyerHemlockHouse where
  runMessage msg (FoyerHemlockHouse attrs) =
    runQueueT $ FoyerHemlockHouse <$> liftRunMessage msg attrs
