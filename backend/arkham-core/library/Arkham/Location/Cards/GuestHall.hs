module Arkham.Location.Cards.GuestHall where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (guestHall)
import Arkham.Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Modifier
import Arkham.Target

newtype GuestHall = GuestHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

guestHall :: LocationCard GuestHall
guestHall = location
  GuestHall
  Cards.guestHall
  1
  (Static 0)
  T
  [Circle, Heart, Star, Square]

instance HasModifiersFor GuestHall where
  getModifiersFor _ (InvestigatorTarget iid) (GuestHall attrs) =
    pure $ toModifiers
      attrs
      [ CannotTakeAction (IsAction Draw)
      | iid `elem` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance RunMessage GuestHall where
  runMessage msg (GuestHall attrs) = GuestHall <$> runMessage msg attrs
