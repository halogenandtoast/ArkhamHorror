module Arkham.Types.Event.Cards.WorkingAHunch where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype WorkingAHunch = WorkingAHunch EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch :: InvestigatorId -> EventId -> WorkingAHunch
workingAHunch iid uuid = WorkingAHunch $ baseAttrs iid uuid "01037"

instance HasModifiersFor env WorkingAHunch where
  getModifiersFor = noModifiersFor

instance HasActions env WorkingAHunch where
  getActions i window (WorkingAHunch attrs) = getActions i window attrs

instance EventRunner env => RunMessage env WorkingAHunch where
  runMessage msg e@(WorkingAHunch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      locationClueCount <- unClueCount <$> getCount currentLocationId
      if locationClueCount > 0
        then e <$ unshiftMessages
          [ DiscoverCluesAtLocation iid currentLocationId 1 Nothing
          , Discard (EventTarget eid)
          ]
        else e <$ unshiftMessages [Discard (EventTarget eid)]
    _ -> WorkingAHunch <$> runMessage msg attrs
