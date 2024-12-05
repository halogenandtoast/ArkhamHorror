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
  getModifiersFor (DonLagorio attrs) = do
    field EnemyLocation attrs.id >>= \case
      Nothing -> pure mempty
      Just loc -> do
        mCounterClockwiseLocationId <- getCounterClockwiseLocation loc
        modifySelf attrs $ case mCounterClockwiseLocationId of
          Nothing -> []
          Just counterClockwiseLocationId -> [HunterConnectedTo counterClockwiseLocationId]

instance RunMessage DonLagorio where
  runMessage msg (DonLagorio attrs) = DonLagorio <$> runMessage msg attrs
