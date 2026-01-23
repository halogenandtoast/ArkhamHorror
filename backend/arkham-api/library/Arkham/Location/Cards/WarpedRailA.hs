module Arkham.Location.Cards.WarpedRailA (warpedRailA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WarpedRailA = WarpedRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedRailA :: LocationCard WarpedRailA
warpedRailA = location WarpedRailA Cards.warpedRailA 2 (PerPlayer 2)

instance HasAbilities WarpedRailA where
  getAbilities (WarpedRailA a) =
    extendRevealed a []

instance RunMessage WarpedRailA where
  runMessage msg (WarpedRailA attrs) = runQueueT $ case msg of
    _ -> WarpedRailA <$> liftRunMessage msg attrs
