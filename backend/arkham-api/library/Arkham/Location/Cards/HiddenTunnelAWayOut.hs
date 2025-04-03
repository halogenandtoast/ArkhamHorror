module Arkham.Location.Cards.HiddenTunnelAWayOut (hiddenTunnelAWayOut) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenTunnelAWayOut = HiddenTunnelAWayOut LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenTunnelAWayOut :: LocationCard HiddenTunnelAWayOut
hiddenTunnelAWayOut = location HiddenTunnelAWayOut Cards.hiddenTunnelAWayOut 0 (Static 0)

instance HasAbilities HiddenTunnelAWayOut where
  getAbilities (HiddenTunnelAWayOut attrs) =
    extendRevealed attrs []

instance RunMessage HiddenTunnelAWayOut where
  runMessage msg (HiddenTunnelAWayOut attrs) = runQueueT $ case msg of
    _ -> HiddenTunnelAWayOut <$> liftRunMessage msg attrs
