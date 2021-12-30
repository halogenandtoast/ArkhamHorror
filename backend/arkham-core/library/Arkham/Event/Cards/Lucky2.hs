module Arkham.Event.Cards.Lucky2 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype Lucky2 = Lucky2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EventCard Lucky2
lucky2 = event Lucky2 Cards.lucky2

instance EventRunner env => RunMessage env Lucky2 where
  runMessage msg e@(Lucky2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ Discard (EventTarget eid)
      , DrawCards iid 1 False
      , CreateEffect "01084" Nothing (EventSource eid) (InvestigatorTarget iid)
      ]
    _ -> Lucky2 <$> runMessage msg attrs
