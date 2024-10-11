module Arkham.Enemy.Cards.DonLagorio (donLagorio, DonLagorio (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers

newtype DonLagorio = DonLagorio EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

donLagorio :: EnemyCard DonLagorio
donLagorio = enemy DonLagorio Cards.donLagorio (4, Static 4, 3) (2, 0)

instance HasModifiersFor DonLagorio where
  getModifiersFor (EnemyTarget eid) (DonLagorio attrs) | eid == toId attrs = do
    field EnemyLocation eid >>= \case
      Nothing -> pure []
      Just loc -> do
        mCounterClockwiseLocationId <- getCounterClockwiseLocation loc
        toModifiers attrs $ case mCounterClockwiseLocationId of
          Nothing -> []
          Just counterClockwiseLocationId ->
            [ HunterConnectedTo counterClockwiseLocationId
            ]
  getModifiersFor _ _ = pure []

instance RunMessage DonLagorio where
  runMessage msg (DonLagorio attrs) = DonLagorio <$> runMessage msg attrs
