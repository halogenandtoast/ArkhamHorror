module Arkham.Event.Cards.HotStreak2 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Target

newtype HotStreak2 = HotStreak2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak2 :: EventCard HotStreak2
hotStreak2 = event HotStreak2 Cards.hotStreak2

instance RunMessage HotStreak2 where
  runMessage msg e@(HotStreak2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId ->
      e <$ pushAll [TakeResources iid 10 (toSource attrs) False, Discard (EventTarget eid)]
    _ -> HotStreak2 <$> runMessage msg attrs
