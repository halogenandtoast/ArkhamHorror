module Arkham.Event.Events.HotStreak2 where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype HotStreak2 = HotStreak2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak2 :: EventCard HotStreak2
hotStreak2 = event HotStreak2 Cards.hotStreak2

instance RunMessage HotStreak2 where
  runMessage msg e@(HotStreak2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid attrs 10
      pure e
    _ -> HotStreak2 <$> liftRunMessage msg attrs
