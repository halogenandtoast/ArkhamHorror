module Arkham.Event.Cards.HotStreak4 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype HotStreak4 = HotStreak4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hotStreak4 :: EventCard HotStreak4
hotStreak4 = event HotStreak4 Cards.hotStreak4

instance RunMessage HotStreak4 where
  runMessage msg e@(HotStreak4 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      push $ TakeResources iid 10 (toSource attrs) False
      pure e
    _ -> HotStreak4 <$> runMessage msg attrs
