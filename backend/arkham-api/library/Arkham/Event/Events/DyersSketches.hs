module Arkham.Event.Events.DyersSketches (dyersSketches) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype DyersSketches = DyersSketches EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersSketches :: EventCard DyersSketches
dyersSketches = event DyersSketches Cards.dyersSketches

instance RunMessage DyersSketches where
  runMessage msg e@(DyersSketches attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCardsIfCan iid attrs 3
      pure e
    _ -> DyersSketches <$> liftRunMessage msg attrs
