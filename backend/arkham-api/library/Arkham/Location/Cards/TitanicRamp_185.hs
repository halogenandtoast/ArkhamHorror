module Arkham.Location.Cards.TitanicRamp_185 (titanicRamp_185) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TitanicRamp_185 = TitanicRamp_185 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_185 :: LocationCard TitanicRamp_185
titanicRamp_185 = location TitanicRamp_185 Cards.titanicRamp_185 0 (Static 0)

instance HasAbilities TitanicRamp_185 where
  getAbilities (TitanicRamp_185 attrs) =
    extendRevealed attrs []

instance RunMessage TitanicRamp_185 where
  runMessage msg (TitanicRamp_185 attrs) = runQueueT $ case msg of
    _ -> TitanicRamp_185 <$> liftRunMessage msg attrs
