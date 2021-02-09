module Arkham.Types.Event.Cards.HotStreak2 where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype HotStreak2 = HotStreak2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak2 :: InvestigatorId -> EventId -> HotStreak2
hotStreak2 iid uuid = HotStreak2 $ baseAttrs iid uuid "50006"

instance HasModifiersFor env HotStreak2 where
  getModifiersFor = noModifiersFor

instance HasActions env HotStreak2 where
  getActions i window (HotStreak2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env HotStreak2 where
  runMessage msg e@(HotStreak2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId ->
      e <$ unshiftMessages
        [TakeResources iid 10 False, Discard (EventTarget eid)]
    _ -> HotStreak2 <$> runMessage msg attrs
