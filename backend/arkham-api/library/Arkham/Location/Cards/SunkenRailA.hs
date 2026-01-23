module Arkham.Location.Cards.SunkenRailA (sunkenRailA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenRailA = SunkenRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenRailA :: LocationCard SunkenRailA
sunkenRailA = location SunkenRailA Cards.sunkenRailA 4 (PerPlayer 2)

instance HasAbilities SunkenRailA where
  getAbilities (SunkenRailA a) =
    extendRevealed a []

instance RunMessage SunkenRailA where
  runMessage msg (SunkenRailA attrs) = runQueueT $ case msg of
    _ -> SunkenRailA <$> liftRunMessage msg attrs
