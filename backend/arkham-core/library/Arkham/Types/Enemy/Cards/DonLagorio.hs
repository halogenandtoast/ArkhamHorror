module Arkham.Types.Enemy.Cards.DonLagorio
  ( donLagorio
  , DonLagorio(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype DonLagorio = DonLagorio EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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

instance EnemyAttrsHasAbilities env => HasAbilities env DonLagorio where
  getAbilities i window (DonLagorio attrs) = getAbilities i window attrs

instance EnemyAttrsRunMessage env => RunMessage env DonLagorio where
  runMessage msg (DonLagorio attrs) = DonLagorio <$> runMessage msg attrs
