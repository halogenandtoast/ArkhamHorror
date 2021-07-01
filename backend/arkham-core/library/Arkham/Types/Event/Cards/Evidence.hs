module Arkham.Types.Event.Cards.Evidence where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

newtype Evidence = Evidence EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidence :: EventCard Evidence
evidence = event Evidence Cards.evidence

instance HasModifiersFor env Evidence where
  getModifiersFor = noModifiersFor

instance HasActions env Evidence where
  getActions i window (Evidence attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Evidence where
  runMessage msg e@(Evidence attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      locationClueCount <- unClueCount <$> getCount currentLocationId
      if locationClueCount > 0
        then e <$ unshiftMessages
          [ DiscoverCluesAtLocation iid currentLocationId 1 Nothing
          , Discard (EventTarget eid)
          ]
        else e <$ unshiftMessages [Discard (EventTarget eid)]
    _ -> Evidence <$> runMessage msg attrs
