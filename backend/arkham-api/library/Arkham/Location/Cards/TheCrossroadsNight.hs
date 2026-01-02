module Arkham.Location.Cards.TheCrossroadsNight (theCrossroadsNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCrossroadsNight = TheCrossroadsNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsNight :: LocationCard TheCrossroadsNight
theCrossroadsNight = symbolLabel $ location TheCrossroadsNight Cards.theCrossroadsNight 0 (Static 0)

instance HasAbilities TheCrossroadsNight where
  getAbilities (TheCrossroadsNight a) =
    extendRevealed a []

instance RunMessage TheCrossroadsNight where
  runMessage msg (TheCrossroadsNight attrs) = runQueueT $ case msg of
    _ -> TheCrossroadsNight <$> liftRunMessage msg attrs
