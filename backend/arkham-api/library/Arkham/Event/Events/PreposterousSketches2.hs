module Arkham.Event.Events.PreposterousSketches2 (preposterousSketches2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype PreposterousSketches2 = PreposterousSketches2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preposterousSketches2 :: EventCard PreposterousSketches2
preposterousSketches2 = event PreposterousSketches2 Cards.preposterousSketches2

instance RunMessage PreposterousSketches2 where
  runMessage msg e@(PreposterousSketches2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCards iid attrs 3
      pure e
    _ -> PreposterousSketches2 <$> liftRunMessage msg attrs
