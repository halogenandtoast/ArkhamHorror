module Arkham.Location.Cards.TheCrossroadsDay (theCrossroadsDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCrossroadsDay = TheCrossroadsDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsDay :: LocationCard TheCrossroadsDay
theCrossroadsDay = symbolLabel $ location TheCrossroadsDay Cards.theCrossroadsDay 0 (Static 0)

instance HasAbilities TheCrossroadsDay where
  getAbilities (TheCrossroadsDay a) =
    extendRevealed a []

instance RunMessage TheCrossroadsDay where
  runMessage msg (TheCrossroadsDay attrs) = runQueueT $ case msg of
    _ -> TheCrossroadsDay <$> liftRunMessage msg attrs
