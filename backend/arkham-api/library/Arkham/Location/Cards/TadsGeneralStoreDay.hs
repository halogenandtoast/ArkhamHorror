module Arkham.Location.Cards.TadsGeneralStoreDay (tadsGeneralStoreDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TadsGeneralStoreDay = TadsGeneralStoreDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tadsGeneralStoreDay :: LocationCard TadsGeneralStoreDay
tadsGeneralStoreDay = symbolLabel $ location TadsGeneralStoreDay Cards.tadsGeneralStoreDay 0 (Static 0)

instance HasAbilities TadsGeneralStoreDay where
  getAbilities (TadsGeneralStoreDay a) =
    extendRevealed a []

instance RunMessage TadsGeneralStoreDay where
  runMessage msg (TadsGeneralStoreDay attrs) = runQueueT $ case msg of
    _ -> TadsGeneralStoreDay <$> liftRunMessage msg attrs
