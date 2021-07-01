module Arkham.Types.Location.Cards.GuestHall where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (guestHall)
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype GuestHall = GuestHall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guestHall :: LocationId -> GuestHall
guestHall = GuestHall . baseAttrs
  Cards.guestHall
  1
  (Static 0)
  T
  [Circle, Heart, Star, Square]

instance HasModifiersFor env GuestHall where
  getModifiersFor _ (InvestigatorTarget iid) (GuestHall attrs) =
    pure $ toModifiers
      attrs
      [ CannotTakeAction (IsAction Draw)
      | iid `elem` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env GuestHall where
  getActions i window (GuestHall attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env GuestHall where
  runMessage msg (GuestHall attrs) = GuestHall <$> runMessage msg attrs
