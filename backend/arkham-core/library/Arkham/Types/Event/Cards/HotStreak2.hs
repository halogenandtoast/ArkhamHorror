module Arkham.Types.Event.Cards.HotStreak2 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype HotStreak2 = HotStreak2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak2 :: EventCard HotStreak2
hotStreak2 = event HotStreak2 Cards.hotStreak2

instance EventRunner env => RunMessage env HotStreak2 where
  runMessage msg e@(HotStreak2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId ->
      e <$ pushAll [TakeResources iid 10 False, Discard (EventTarget eid)]
    _ -> HotStreak2 <$> runMessage msg attrs
