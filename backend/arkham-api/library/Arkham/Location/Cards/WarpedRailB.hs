module Arkham.Location.Cards.WarpedRailB (warpedRailB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WarpedRailB = WarpedRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedRailB :: LocationCard WarpedRailB
warpedRailB = symbolLabel $ location WarpedRailB Cards.warpedRailB 2 (PerPlayer 2)

instance HasAbilities WarpedRailB where
  getAbilities (WarpedRailB a) =
    extendRevealed a []

instance RunMessage WarpedRailB where
  runMessage msg (WarpedRailB attrs) = runQueueT $ case msg of
    _ -> WarpedRailB <$> liftRunMessage msg attrs
