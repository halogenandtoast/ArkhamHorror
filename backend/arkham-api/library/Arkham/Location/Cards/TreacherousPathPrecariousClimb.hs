module Arkham.Location.Cards.TreacherousPathPrecariousClimb (treacherousPathPrecariousClimb) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPathPrecariousClimb = TreacherousPathPrecariousClimb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathPrecariousClimb :: LocationCard TreacherousPathPrecariousClimb
treacherousPathPrecariousClimb = location TreacherousPathPrecariousClimb Cards.treacherousPathPrecariousClimb 0 (Static 1)

-- TODO: abilities

instance RunMessage TreacherousPathPrecariousClimb where
  runMessage msg (TreacherousPathPrecariousClimb attrs) = runQueueT $ TreacherousPathPrecariousClimb <$> liftRunMessage msg attrs
