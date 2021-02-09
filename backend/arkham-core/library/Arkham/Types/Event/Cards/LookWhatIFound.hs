module Arkham.Types.Event.Cards.LookWhatIFound where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound :: InvestigatorId -> EventId -> LookWhatIFound
lookWhatIFound iid uuid = LookWhatIFound $ baseAttrs iid uuid "01079"

instance HasModifiersFor env LookWhatIFound where
  getModifiersFor = noModifiersFor

instance HasActions env LookWhatIFound where
  getActions i window (LookWhatIFound attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId iid
      e <$ unshiftMessages
        [InvestigatorDiscoverClues iid lid 2 Nothing, Discard (EventTarget eid)]
    _ -> LookWhatIFound <$> runMessage msg attrs
