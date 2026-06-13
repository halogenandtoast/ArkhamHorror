module Arkham.Location.Cards.EastAntechamber (eastAntechamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EastAntechamber = EastAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eastAntechamber :: LocationCard EastAntechamber
eastAntechamber = location EastAntechamber Cards.eastAntechamber 3 (Static 1)

-- TODO: abilities

instance RunMessage EastAntechamber where
  runMessage msg (EastAntechamber attrs) = runQueueT $ EastAntechamber <$> liftRunMessage msg attrs
