module Arkham.Location.Cards.TadsGeneralStoreNight (tadsGeneralStoreNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TadsGeneralStoreNight = TadsGeneralStoreNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tadsGeneralStoreNight :: LocationCard TadsGeneralStoreNight
tadsGeneralStoreNight = symbolLabel $ location TadsGeneralStoreNight Cards.tadsGeneralStoreNight 0 (Static 0)

instance HasAbilities TadsGeneralStoreNight where
  getAbilities (TadsGeneralStoreNight a) =
    extendRevealed a []

instance RunMessage TadsGeneralStoreNight where
  runMessage msg (TadsGeneralStoreNight attrs) = runQueueT $ case msg of
    _ -> TadsGeneralStoreNight <$> liftRunMessage msg attrs
