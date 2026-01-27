module Arkham.Event.Events.GatherIntel (gatherIntel) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype GatherIntel = GatherIntel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gatherIntel :: EventCard GatherIntel
gatherIntel = event GatherIntel Cards.gatherIntel

instance RunMessage GatherIntel where
  runMessage msg e@(GatherIntel attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCards iid attrs 2
      pure e
    _ -> GatherIntel <$> liftRunMessage msg attrs
