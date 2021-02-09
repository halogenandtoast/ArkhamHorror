module Arkham.Types.Event.Cards.Lucky2 where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Lucky2 = Lucky2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: InvestigatorId -> EventId -> Lucky2
lucky2 iid uuid = Lucky2 $ baseAttrs iid uuid "01084"

instance HasModifiersFor env Lucky2 where
  getModifiersFor = noModifiersFor

instance HasActions env Lucky2 where
  getActions i window (Lucky2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky2 where
  runMessage msg e@(Lucky2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ Discard (EventTarget eid)
      , DrawCards iid 1 False
      , CreateEffect "01084" Nothing (EventSource eid) (InvestigatorTarget iid)
      ]
    _ -> Lucky2 <$> runMessage msg attrs
