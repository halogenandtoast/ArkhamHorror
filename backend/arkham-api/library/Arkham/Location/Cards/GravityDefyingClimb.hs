module Arkham.Location.Cards.GravityDefyingClimb (gravityDefyingClimb) where

import Arkham.Cost
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GravityDefyingClimb = GravityDefyingClimb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravityDefyingClimb :: LocationCard GravityDefyingClimb
gravityDefyingClimb =
  symbolLabel
    $ locationWith
      GravityDefyingClimb
      Cards.gravityDefyingClimb
      0
      (Static 0)
      (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) "The Knotted Tower")

instance HasAbilities GravityDefyingClimb where
  getAbilities (GravityDefyingClimb a) =
    extendRevealed a []

instance RunMessage GravityDefyingClimb where
  runMessage msg (GravityDefyingClimb attrs) = runQueueT $ case msg of
    _ -> GravityDefyingClimb <$> liftRunMessage msg attrs
