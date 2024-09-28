module Arkham.Location.Cards.DesolateCoastline (desolateCoastline, DesolateCoastline (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype DesolateCoastline = DesolateCoastline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desolateCoastline :: LocationCard DesolateCoastline
desolateCoastline = locationWith DesolateCoastline Cards.desolateCoastline 2 (Static 1) connectsToAdjacent

instance RunMessage DesolateCoastline where
  runMessage msg (DesolateCoastline attrs) = runQueueT $ case msg of
    _ -> DesolateCoastline <$> liftRunMessage msg attrs
