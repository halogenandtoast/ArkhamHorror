module Arkham.Location.Cards.TanneriesAbandoned (tanneriesAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TanneriesAbandoned = TanneriesAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tanneriesAbandoned :: LocationCard TanneriesAbandoned
tanneriesAbandoned = symbolLabel $ location TanneriesAbandoned Cards.tanneriesAbandoned 3 (Static 0)

instance HasAbilities TanneriesAbandoned where
  getAbilities (TanneriesAbandoned attrs) =
    extendRevealed attrs []

instance RunMessage TanneriesAbandoned where
  runMessage msg (TanneriesAbandoned attrs) = runQueueT $ case msg of
    _ -> TanneriesAbandoned <$> liftRunMessage msg attrs
