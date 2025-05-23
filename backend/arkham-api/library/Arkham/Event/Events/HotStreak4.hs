module Arkham.Event.Events.HotStreak4 (hotStreak4) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype HotStreak4 = HotStreak4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak4 :: EventCard HotStreak4
hotStreak4 = event HotStreak4 Cards.hotStreak4

instance RunMessage HotStreak4 where
  runMessage msg e@(HotStreak4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid attrs 10
      pure e
    _ -> HotStreak4 <$> liftRunMessage msg attrs
