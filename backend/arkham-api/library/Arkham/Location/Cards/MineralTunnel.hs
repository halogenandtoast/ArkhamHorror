module Arkham.Location.Cards.MineralTunnel (mineralTunnel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MineralTunnel = MineralTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mineralTunnel :: LocationCard MineralTunnel
mineralTunnel = locationWith MineralTunnel Cards.mineralTunnel 0 (PerPlayer 1) connectsToAdjacent

instance HasAbilities MineralTunnel where
  getAbilities (MineralTunnel a) =
    extendRevealed a []

instance RunMessage MineralTunnel where
  runMessage msg (MineralTunnel attrs) = runQueueT $ case msg of
    _ -> MineralTunnel <$> liftRunMessage msg attrs
