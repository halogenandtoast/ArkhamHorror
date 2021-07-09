module Arkham.Types.Event.Cards.DrawnToTheFlame where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance HasModifiersFor env DrawnToTheFlame where
  getModifiersFor = noModifiersFor

instance HasActions env DrawnToTheFlame where
  getActions i window (DrawnToTheFlame attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ InvestigatorDrawEncounterCard iid
      , InvestigatorDiscoverCluesAtTheirLocation iid 2 Nothing
      , Discard (EventTarget eid)
      ]
    _ -> DrawnToTheFlame <$> runMessage msg attrs
