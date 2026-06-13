module Arkham.Location.Cards.TreacherousPathSlickSteps (treacherousPathSlickSteps) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPathSlickSteps = TreacherousPathSlickSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathSlickSteps :: LocationCard TreacherousPathSlickSteps
treacherousPathSlickSteps = location TreacherousPathSlickSteps Cards.treacherousPathSlickSteps 0 (Static 1)

-- TODO: abilities

instance RunMessage TreacherousPathSlickSteps where
  runMessage msg (TreacherousPathSlickSteps attrs) = runQueueT $ TreacherousPathSlickSteps <$> liftRunMessage msg attrs
