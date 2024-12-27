module Arkham.Location.Cards.SteepIncline (steepIncline) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SteepIncline = SteepIncline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steepIncline :: LocationCard SteepIncline
steepIncline = locationWith SteepIncline Cards.steepIncline 2 (PerPlayer 2) (connectsToL .~ adjacentLocations)

instance HasAbilities SteepIncline where
  getAbilities (SteepIncline attrs) =
    extendRevealed attrs []

instance RunMessage SteepIncline where
  runMessage msg (SteepIncline attrs) = runQueueT $ case msg of
    _ -> SteepIncline <$> liftRunMessage msg attrs
