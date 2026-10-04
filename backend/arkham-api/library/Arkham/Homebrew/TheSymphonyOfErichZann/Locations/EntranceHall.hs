module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.EntranceHall (entranceHall) where

import Arkham.Location.Helpers qualified as LH
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype EntranceHall = EntranceHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entranceHall :: LocationCard EntranceHall
entranceHall = location EntranceHall Cards.entranceHall 2 (Static 1)

instance HasAbilities EntranceHall where
  -- "[action]: Resign. You flee the theatre before the music consumes you."
  getAbilities (EntranceHall a) = extend1 a $ LH.resignAction a

instance RunMessage EntranceHall where
  runMessage msg (EntranceHall attrs) = runQueueT $ EntranceHall <$> liftRunMessage msg attrs
