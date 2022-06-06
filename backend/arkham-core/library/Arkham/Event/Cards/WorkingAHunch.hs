module Arkham.Event.Cards.WorkingAHunch where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Query
import Arkham.Target

newtype WorkingAHunch = WorkingAHunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch :: EventCard WorkingAHunch
workingAHunch = event WorkingAHunch Cards.workingAHunch

instance EventRunner env => RunMessage WorkingAHunch where
  runMessage msg e@(WorkingAHunch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      locationClueCount <- unClueCount <$> getCount currentLocationId
      if locationClueCount > 0
        then e <$ pushAll
          [ DiscoverCluesAtLocation iid currentLocationId 1 Nothing
          , Discard (EventTarget eid)
          ]
        else e <$ pushAll [Discard (EventTarget eid)]
    _ -> WorkingAHunch <$> runMessage msg attrs
