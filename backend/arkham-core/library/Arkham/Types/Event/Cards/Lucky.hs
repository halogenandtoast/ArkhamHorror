module Arkham.Types.Event.Cards.Lucky where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype Lucky = Lucky EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance HasModifiersFor env Lucky where
  getModifiersFor = noModifiersFor

instance HasActions env Lucky where
  getActions i window (Lucky attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky where
  runMessage msg e@(Lucky attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ Discard (EventTarget eid)
      , CreateEffect "01080" Nothing (EventSource eid) (InvestigatorTarget iid)
      ]
    _ -> Lucky <$> runMessage msg attrs
