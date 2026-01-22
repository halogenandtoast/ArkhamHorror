module Arkham.Location.Cards.WarpedRail (warpedRail) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WarpedRail = WarpedRail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedRail :: LocationCard WarpedRail
warpedRail = location WarpedRail Cards.warpedRail 2 (PerPlayer 2)

instance HasAbilities WarpedRail where
  getAbilities (WarpedRail a) =
    extendRevealed a []

instance RunMessage WarpedRail where
  runMessage msg (WarpedRail attrs) = runQueueT $ case msg of
    _ -> WarpedRail <$> liftRunMessage msg attrs
