module Arkham.Location.Cards.TreacherousPathShallowDen (treacherousPathShallowDen) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPathShallowDen = TreacherousPathShallowDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathShallowDen :: LocationCard TreacherousPathShallowDen
treacherousPathShallowDen = location TreacherousPathShallowDen Cards.treacherousPathShallowDen 0 (Static 1)

-- TODO: abilities

instance RunMessage TreacherousPathShallowDen where
  runMessage msg (TreacherousPathShallowDen attrs) = runQueueT $ TreacherousPathShallowDen <$> liftRunMessage msg attrs
