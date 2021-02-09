module Arkham.Types.Event.Cards.MindOverMatter where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: InvestigatorId -> EventId -> MindOverMatter
mindOverMatter iid uuid = MindOverMatter $ baseAttrs iid uuid "01036"

instance HasModifiersFor env MindOverMatter where
  getModifiersFor = noModifiersFor

instance HasActions env MindOverMatter where
  getActions i window (MindOverMatter attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindOverMatter where
  runMessage msg e@(MindOverMatter attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessages
        [ CreateEffect "01036" Nothing (toSource attrs) (InvestigatorTarget iid)
        , Discard (EventTarget eid)
        ]
    _ -> MindOverMatter <$> runMessage msg attrs
