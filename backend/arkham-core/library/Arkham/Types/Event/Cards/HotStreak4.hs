module Arkham.Types.Event.Cards.HotStreak4 where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype HotStreak4 = HotStreak4 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak4 :: InvestigatorId -> EventId -> HotStreak4
hotStreak4 iid uuid = HotStreak4 $ baseAttrs iid uuid "01057"

instance HasModifiersFor env HotStreak4 where
  getModifiersFor = noModifiersFor

instance HasActions env HotStreak4 where
  getActions i window (HotStreak4 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env HotStreak4 where
  runMessage msg e@(HotStreak4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId ->
      e <$ unshiftMessages
        [TakeResources iid 10 False, Discard (EventTarget eid)]
    _ -> HotStreak4 <$> runMessage msg attrs
