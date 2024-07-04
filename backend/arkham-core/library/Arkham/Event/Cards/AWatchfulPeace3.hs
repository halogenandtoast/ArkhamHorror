module Arkham.Event.Cards.AWatchfulPeace3 (aWatchfulPeace3, AWatchfulPeace3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype AWatchfulPeace3 = AWatchfulPeace3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWatchfulPeace3 :: EventCard AWatchfulPeace3
aWatchfulPeace3 = event AWatchfulPeace3 Cards.aWatchfulPeace3

instance RunMessage AWatchfulPeace3 where
  runMessage msg e@(AWatchfulPeace3 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      don't AllDrawEncounterCard
      pure e
    _ -> AWatchfulPeace3 <$> liftRunMessage msg attrs
