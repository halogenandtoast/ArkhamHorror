module Arkham.Location.Cards.OvergrownTunnel (overgrownTunnel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OvergrownTunnel = OvergrownTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownTunnel :: LocationCard OvergrownTunnel
overgrownTunnel = locationWith OvergrownTunnel Cards.overgrownTunnel 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities OvergrownTunnel where
  getAbilities (OvergrownTunnel a) =
    extendRevealed a []

instance RunMessage OvergrownTunnel where
  runMessage msg (OvergrownTunnel attrs) = runQueueT $ case msg of
    _ -> OvergrownTunnel <$> liftRunMessage msg attrs
