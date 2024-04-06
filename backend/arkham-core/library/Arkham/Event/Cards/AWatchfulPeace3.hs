module Arkham.Event.Cards.AWatchfulPeace3 (aWatchfulPeace3, AWatchfulPeace3 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Prelude

newtype AWatchfulPeace3 = AWatchfulPeace3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWatchfulPeace3 :: EventCard AWatchfulPeace3
aWatchfulPeace3 = event AWatchfulPeace3 Cards.aWatchfulPeace3

instance RunMessage AWatchfulPeace3 where
  runMessage msg e@(AWatchfulPeace3 attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      popMessageMatching_ \case
        AllDrawEncounterCard -> True
        _ -> False
      pure e
    _ -> AWatchfulPeace3 <$> runMessage msg attrs
