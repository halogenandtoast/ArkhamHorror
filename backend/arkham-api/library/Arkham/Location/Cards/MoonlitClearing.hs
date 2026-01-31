module Arkham.Location.Cards.MoonlitClearing (moonlitClearing) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitClearing = MoonlitClearing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitClearing :: LocationCard MoonlitClearing
moonlitClearing = locationWith MoonlitClearing Cards.moonlitClearing 1 (PerPlayer 1) connectsToAdjacent

instance HasAbilities MoonlitClearing where
  getAbilities (MoonlitClearing a) =
    extendRevealed a []

instance RunMessage MoonlitClearing where
  runMessage msg (MoonlitClearing attrs) = runQueueT $ case msg of
    _ -> MoonlitClearing <$> liftRunMessage msg attrs
