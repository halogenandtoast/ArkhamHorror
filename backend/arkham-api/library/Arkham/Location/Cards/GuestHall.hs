module Arkham.Location.Cards.GuestHall where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (guestHall)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GuestHall = GuestHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

guestHall :: LocationCard GuestHall
guestHall = location GuestHall Cards.guestHall 1 (Static 0)

instance HasModifiersFor GuestHall where
  getModifiersFor (GuestHall a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [CannotTakeAction #draw]

instance RunMessage GuestHall where
  runMessage msg (GuestHall attrs) = GuestHall <$> runMessage msg attrs
