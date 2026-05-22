module Arkham.Location.Cards.LarvalTunnel (larvalTunnel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LarvalTunnel = LarvalTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

larvalTunnel :: LocationCard LarvalTunnel
larvalTunnel = symbolLabel $ location LarvalTunnel Cards.larvalTunnel 2 (PerPlayer 3)

instance HasAbilities LarvalTunnel where
  getAbilities (LarvalTunnel a) =
    extendRevealed a []

instance RunMessage LarvalTunnel where
  runMessage msg (LarvalTunnel attrs) = runQueueT $ case msg of
    _ -> LarvalTunnel <$> liftRunMessage msg attrs
