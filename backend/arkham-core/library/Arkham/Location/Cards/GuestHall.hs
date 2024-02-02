module Arkham.Location.Cards.GuestHall where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (guestHall)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype GuestHall = GuestHall LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

guestHall :: LocationCard GuestHall
guestHall = location GuestHall Cards.guestHall 1 (Static 0)

instance HasModifiersFor GuestHall where
  getModifiersFor (InvestigatorTarget iid) (GuestHall attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [CannotTakeAction #draw | here]
  getModifiersFor _ _ = pure []

instance RunMessage GuestHall where
  runMessage msg (GuestHall attrs) = GuestHall <$> runMessage msg attrs
