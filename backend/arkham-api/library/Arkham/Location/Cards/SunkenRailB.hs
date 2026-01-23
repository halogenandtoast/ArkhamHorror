module Arkham.Location.Cards.SunkenRailB (sunkenRailB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenRailB = SunkenRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenRailB :: LocationCard SunkenRailB
sunkenRailB = symbolLabel $ location SunkenRailB Cards.sunkenRailB 4 (PerPlayer 2)

instance HasAbilities SunkenRailB where
  getAbilities (SunkenRailB a) =
    extendRevealed a []

instance RunMessage SunkenRailB where
  runMessage msg (SunkenRailB attrs) = runQueueT $ case msg of
    _ -> SunkenRailB <$> liftRunMessage msg attrs
