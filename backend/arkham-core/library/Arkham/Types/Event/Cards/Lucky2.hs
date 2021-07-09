module Arkham.Types.Event.Cards.Lucky2 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype Lucky2 = Lucky2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EventCard Lucky2
lucky2 = event Lucky2 Cards.lucky2

instance HasModifiersFor env Lucky2 where
  getModifiersFor = noModifiersFor

instance HasActions env Lucky2 where
  getActions i window (Lucky2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky2 where
  runMessage msg e@(Lucky2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ Discard (EventTarget eid)
      , DrawCards iid 1 False
      , CreateEffect "01084" Nothing (EventSource eid) (InvestigatorTarget iid)
      ]
    _ -> Lucky2 <$> runMessage msg attrs
