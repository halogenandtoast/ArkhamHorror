module Arkham.Location.Cards.ScarletHallsLair (scarletHallsLair) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ScarletHallsLair = ScarletHallsLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scarletHallsLair :: LocationCard ScarletHallsLair
scarletHallsLair = location ScarletHallsLair Cards.scarletHallsLair 0 (Static 0)

instance HasAbilities ScarletHallsLair where
  getAbilities (ScarletHallsLair a) =
    extendRevealed a []

instance RunMessage ScarletHallsLair where
  runMessage msg (ScarletHallsLair attrs) = runQueueT $ case msg of
    _ -> ScarletHallsLair <$> liftRunMessage msg attrs
