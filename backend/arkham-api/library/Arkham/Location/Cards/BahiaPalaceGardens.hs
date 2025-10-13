module Arkham.Location.Cards.BahiaPalaceGardens (bahiaPalaceGardens) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BahiaPalaceGardens = BahiaPalaceGardens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bahiaPalaceGardens :: LocationCard BahiaPalaceGardens
bahiaPalaceGardens = symbolLabel $ location BahiaPalaceGardens Cards.bahiaPalaceGardens 4 (PerPlayer 2)

instance HasAbilities BahiaPalaceGardens where
  getAbilities (BahiaPalaceGardens attrs) =
    extendRevealed attrs []

instance RunMessage BahiaPalaceGardens where
  runMessage msg (BahiaPalaceGardens attrs) = runQueueT $ case msg of
    _ -> BahiaPalaceGardens <$> liftRunMessage msg attrs
