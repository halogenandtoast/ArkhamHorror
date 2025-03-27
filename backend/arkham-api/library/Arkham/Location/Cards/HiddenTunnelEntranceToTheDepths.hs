module Arkham.Location.Cards.HiddenTunnelEntranceToTheDepths (hiddenTunnelEntranceToTheDepths) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenTunnelEntranceToTheDepths = HiddenTunnelEntranceToTheDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenTunnelEntranceToTheDepths :: LocationCard HiddenTunnelEntranceToTheDepths
hiddenTunnelEntranceToTheDepths =
  locationWith
    HiddenTunnelEntranceToTheDepths
    Cards.hiddenTunnelEntranceToTheDepths
    0
    (PerPlayer 2)
    ((shroudL .~ Nothing) . connectsToAdjacent)

instance HasAbilities HiddenTunnelEntranceToTheDepths where
  getAbilities (HiddenTunnelEntranceToTheDepths attrs) =
    extendRevealed attrs []

instance RunMessage HiddenTunnelEntranceToTheDepths where
  runMessage msg (HiddenTunnelEntranceToTheDepths attrs) = runQueueT $ case msg of
    _ -> HiddenTunnelEntranceToTheDepths <$> liftRunMessage msg attrs
