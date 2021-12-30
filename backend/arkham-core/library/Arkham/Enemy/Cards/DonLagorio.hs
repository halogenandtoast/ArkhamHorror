module Arkham.Enemy.Cards.DonLagorio
  ( donLagorio
  , DonLagorio(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Target

newtype DonLagorio = DonLagorio EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

donLagorio :: EnemyCard DonLagorio
donLagorio = enemy DonLagorio Cards.donLagorio (4, Static 4, 3) (2, 0)

-- Since we will check the enemies location here, we need to make sure don has
-- spawned before checking for modifiers
instance (HasSet ConnectedLocationId env LocationId, HasSet LocationId env ()) => HasModifiersFor env DonLagorio where
  getModifiersFor _ (EnemyTarget eid) (DonLagorio attrs)
    | eid == toId attrs && spawned attrs = do
      counterClockwiseLocationId <- getCounterClockwiseLocation
        (enemyLocation attrs)
      pure $ toModifiers attrs [HunterConnectedTo counterClockwiseLocationId]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env DonLagorio where
  runMessage msg (DonLagorio attrs) = DonLagorio <$> runMessage msg attrs
