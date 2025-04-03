module Arkham.Location.Cards.TitanicRamp_183 (titanicRamp_183) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TitanicRamp_183 = TitanicRamp_183 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_183 :: LocationCard TitanicRamp_183
titanicRamp_183 = location TitanicRamp_183 Cards.titanicRamp_183 0 (Static 0)

instance HasAbilities TitanicRamp_183 where
  getAbilities (TitanicRamp_183 attrs) =
    extendRevealed attrs []

instance RunMessage TitanicRamp_183 where
  runMessage msg (TitanicRamp_183 attrs) = runQueueT $ case msg of
    _ -> TitanicRamp_183 <$> liftRunMessage msg attrs
