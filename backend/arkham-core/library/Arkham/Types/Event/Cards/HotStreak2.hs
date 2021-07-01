module Arkham.Types.Event.Cards.HotStreak2 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype HotStreak2 = HotStreak2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak2 :: EventCard HotStreak2
hotStreak2 = event HotStreak2 Cards.hotStreak2

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
