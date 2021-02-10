module Arkham.Types.Event.Cards.DrawnToTheFlame where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: InvestigatorId -> EventId -> DrawnToTheFlame
drawnToTheFlame iid uuid = DrawnToTheFlame $ baseAttrs iid uuid "01064"

instance HasModifiersFor env DrawnToTheFlame where
  getModifiersFor = noModifiersFor

instance HasActions env DrawnToTheFlame where
  getActions i window (DrawnToTheFlame attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ InvestigatorDrawEncounterCard iid
      , InvestigatorDiscoverCluesAtTheirLocation iid 2 Nothing
      , Discard (EventTarget eid)
      ]
    _ -> DrawnToTheFlame <$> runMessage msg attrs
