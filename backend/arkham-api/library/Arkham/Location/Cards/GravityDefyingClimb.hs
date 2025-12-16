module Arkham.Location.Cards.GravityDefyingClimb (gravityDefyingClimb) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GravityDefyingClimb = GravityDefyingClimb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravityDefyingClimb :: LocationCard GravityDefyingClimb
gravityDefyingClimb = symbolLabel $ location GravityDefyingClimb Cards.gravityDefyingClimb 0 (Static 0)

instance HasAbilities GravityDefyingClimb where
  getAbilities (GravityDefyingClimb a) =
    extendRevealed a []

instance RunMessage GravityDefyingClimb where
  runMessage msg (GravityDefyingClimb attrs) = runQueueT $ case msg of
    _ -> GravityDefyingClimb <$> liftRunMessage msg attrs
