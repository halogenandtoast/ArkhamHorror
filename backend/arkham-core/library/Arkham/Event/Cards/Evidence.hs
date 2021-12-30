module Arkham.Event.Cards.Evidence where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Query
import Arkham.Target

newtype Evidence = Evidence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidence :: EventCard Evidence
evidence = event Evidence Cards.evidence

instance EventRunner env => RunMessage env Evidence where
  runMessage msg e@(Evidence attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      locationClueCount <- unClueCount <$> getCount currentLocationId
      if locationClueCount > 0
        then e <$ pushAll
          [ DiscoverCluesAtLocation iid currentLocationId 1 Nothing
          , Discard (EventTarget eid)
          ]
        else e <$ pushAll [Discard (EventTarget eid)]
    _ -> Evidence <$> runMessage msg attrs
