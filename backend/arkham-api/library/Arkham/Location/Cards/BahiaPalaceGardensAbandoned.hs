module Arkham.Location.Cards.BahiaPalaceGardensAbandoned (bahiaPalaceGardensAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BahiaPalaceGardensAbandoned = BahiaPalaceGardensAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bahiaPalaceGardensAbandoned :: LocationCard BahiaPalaceGardensAbandoned
bahiaPalaceGardensAbandoned = symbolLabel $ location BahiaPalaceGardensAbandoned Cards.bahiaPalaceGardensAbandoned 4 (Static 0)

instance HasAbilities BahiaPalaceGardensAbandoned where
  getAbilities (BahiaPalaceGardensAbandoned attrs) =
    extendRevealed attrs []

instance RunMessage BahiaPalaceGardensAbandoned where
  runMessage msg (BahiaPalaceGardensAbandoned attrs) = runQueueT $ case msg of
    _ -> BahiaPalaceGardensAbandoned <$> liftRunMessage msg attrs
