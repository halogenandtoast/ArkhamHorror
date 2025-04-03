module Arkham.Location.Cards.TitanicRamp_184 (titanicRamp_184) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TitanicRamp_184 = TitanicRamp_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_184 :: LocationCard TitanicRamp_184
titanicRamp_184 = location TitanicRamp_184 Cards.titanicRamp_184 0 (Static 0)

instance HasAbilities TitanicRamp_184 where
  getAbilities (TitanicRamp_184 attrs) =
    extendRevealed attrs []

instance RunMessage TitanicRamp_184 where
  runMessage msg (TitanicRamp_184 attrs) = runQueueT $ case msg of
    _ -> TitanicRamp_184 <$> liftRunMessage msg attrs
