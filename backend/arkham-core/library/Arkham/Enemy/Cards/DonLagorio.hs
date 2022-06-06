module Arkham.Enemy.Cards.DonLagorio
  ( donLagorio
  , DonLagorio(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Target

newtype DonLagorio = DonLagorio EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

donLagorio :: EnemyCard DonLagorio
donLagorio = enemy DonLagorio Cards.donLagorio (4, Static 4, 3) (2, 0)

-- Since we will check the enemies location here, we need to make sure don has
-- spawned before checking for modifiers
instance (HasSet ConnectedLocationId env LocationId, HasSet LocationId env ()) => HasModifiersFor DonLagorio where
  getModifiersFor _ (EnemyTarget eid) (DonLagorio attrs) | eid == toId attrs =
    case enemyLocation attrs of
      Nothing -> pure []
      Just loc -> do
        counterClockwiseLocationId <- getCounterClockwiseLocation loc
        pure $ toModifiers attrs [HunterConnectedTo counterClockwiseLocationId]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage DonLagorio where
  runMessage msg (DonLagorio attrs) = DonLagorio <$> runMessage msg attrs
