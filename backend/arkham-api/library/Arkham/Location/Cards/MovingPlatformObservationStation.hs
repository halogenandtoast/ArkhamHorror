module Arkham.Location.Cards.MovingPlatformObservationStation (movingPlatformObservationStation) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MovingPlatformObservationStation = MovingPlatformObservationStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

movingPlatformObservationStation :: LocationCard MovingPlatformObservationStation
movingPlatformObservationStation = location MovingPlatformObservationStation Cards.movingPlatformObservationStation 2 (Static 2)

-- TODO: abilities

instance RunMessage MovingPlatformObservationStation where
  runMessage msg (MovingPlatformObservationStation attrs) = runQueueT $ MovingPlatformObservationStation <$> liftRunMessage msg attrs
