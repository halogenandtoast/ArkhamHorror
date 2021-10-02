module Arkham.Types.Event.Cards.DrawnToTheFlame where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance EventRunner env => RunMessage env DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ InvestigatorDrawEncounterCard iid
      , InvestigatorDiscoverCluesAtTheirLocation iid 2 Nothing
      , Discard (EventTarget eid)
      ]
    _ -> DrawnToTheFlame <$> runMessage msg attrs
