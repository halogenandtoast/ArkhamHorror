module Arkham.Location.Cards.SlotMachines (slotMachines) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SlotMachines = SlotMachines LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slotMachines :: LocationCard SlotMachines
slotMachines = symbolLabel $ location SlotMachines Cards.slotMachines 0 (Static 0)

instance HasAbilities SlotMachines where
  getAbilities (SlotMachines attrs) =
    extendRevealed attrs []

instance RunMessage SlotMachines where
  runMessage msg (SlotMachines attrs) = runQueueT $ case msg of
    _ -> SlotMachines <$> liftRunMessage msg attrs
