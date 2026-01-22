module Arkham.Location.Cards.SunkenRail (sunkenRail) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenRail = SunkenRail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenRail :: LocationCard SunkenRail
sunkenRail = location SunkenRail Cards.sunkenRail 4 (PerPlayer 2)

instance HasAbilities SunkenRail where
  getAbilities (SunkenRail a) =
    extendRevealed a []

instance RunMessage SunkenRail where
  runMessage msg (SunkenRail attrs) = runQueueT $ case msg of
    _ -> SunkenRail <$> liftRunMessage msg attrs
