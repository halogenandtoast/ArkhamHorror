module Arkham.Location.Cards.TitanicRamp_182 (titanicRamp_182) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TitanicRamp_182 = TitanicRamp_182 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_182 :: LocationCard TitanicRamp_182
titanicRamp_182 = location TitanicRamp_182 Cards.titanicRamp_182 0 (Static 0)

instance HasAbilities TitanicRamp_182 where
  getAbilities (TitanicRamp_182 attrs) =
    extendRevealed attrs []

instance RunMessage TitanicRamp_182 where
  runMessage msg (TitanicRamp_182 attrs) = runQueueT $ case msg of
    _ -> TitanicRamp_182 <$> liftRunMessage msg attrs
