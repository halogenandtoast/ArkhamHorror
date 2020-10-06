{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Evidence where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype Evidence = Evidence Attrs
  deriving newtype (Show, ToJSON, FromJSON)

evidence :: InvestigatorId -> EventId -> Evidence
evidence iid uuid = Evidence $ baseAttrs iid uuid "01022"

instance HasActions env investigator Evidence where
  getActions i window (Evidence attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Evidence where
  runMessage msg (Evidence attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- asks (getId @LocationId iid)
      locationClueCount <- unClueCount <$> asks (getCount currentLocationId)
      if locationClueCount > 0
        then unshiftMessages
          [ DiscoverCluesAtLocation iid currentLocationId 1
          , Discard (EventTarget eid)
          ]
        else unshiftMessages [Discard (EventTarget eid)]
      Evidence <$> runMessage msg (attrs & resolved .~ True)
    _ -> Evidence <$> runMessage msg attrs
