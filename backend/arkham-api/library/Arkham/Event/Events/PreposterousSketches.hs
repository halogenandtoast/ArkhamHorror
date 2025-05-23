module Arkham.Event.Events.PreposterousSketches (preposterousSketches) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype PreposterousSketches = PreposterousSketches EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preposterousSketches :: EventCard PreposterousSketches
preposterousSketches = event PreposterousSketches Cards.preposterousSketches

instance RunMessage PreposterousSketches where
  runMessage msg e@(PreposterousSketches attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCards iid attrs 3
      pure e
    _ -> PreposterousSketches <$> liftRunMessage msg attrs
