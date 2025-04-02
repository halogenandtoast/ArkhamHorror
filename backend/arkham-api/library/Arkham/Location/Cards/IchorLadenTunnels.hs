module Arkham.Location.Cards.IchorLadenTunnels (ichorLadenTunnels) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IchorLadenTunnels = IchorLadenTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichorLadenTunnels :: LocationCard IchorLadenTunnels
ichorLadenTunnels = location IchorLadenTunnels Cards.ichorLadenTunnels 0 (Static 0)

instance HasAbilities IchorLadenTunnels where
  getAbilities (IchorLadenTunnels attrs) =
    extendRevealed attrs []

instance RunMessage IchorLadenTunnels where
  runMessage msg (IchorLadenTunnels attrs) = runQueueT $ case msg of
    _ -> IchorLadenTunnels <$> liftRunMessage msg attrs
