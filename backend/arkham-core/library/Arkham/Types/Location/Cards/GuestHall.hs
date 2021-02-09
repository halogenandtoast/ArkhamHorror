module Arkham.Types.Location.Cards.GuestHall where


import Arkham.Types.Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner

newtype GuestHall = GuestHall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guestHall :: GuestHall
guestHall = GuestHall $ baseAttrs
  "50014"
  (Name "Guest Hall" Nothing)
  EncounterSet.ReturnToTheGathering
  1
  (Static 0)
  T
  [Circle, Heart, Star, Square]
  mempty

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
