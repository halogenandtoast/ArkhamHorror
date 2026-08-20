module Arkham.Location.Cards.TheInnsmouthConspiracy.IntoTheMaelstrom.GatewayToYhanthlei (gatewayToYhanthlei, GatewayToYhanthlei (..)) where

import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype GatewayToYhanthlei = GatewayToYhanthlei LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gatewayToYhanthlei :: LocationCard GatewayToYhanthlei
gatewayToYhanthlei =
  locationWith GatewayToYhanthlei Cards.gatewayToYhanthlei 2 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ PartiallyFlooded)

instance HasAbilities GatewayToYhanthlei where
  getAbilities (GatewayToYhanthlei attrs) =
    extendRevealed attrs []

instance RunMessage GatewayToYhanthlei where
  runMessage msg (GatewayToYhanthlei attrs) = runQueueT $ case msg of
    _ -> GatewayToYhanthlei <$> liftRunMessage msg attrs
