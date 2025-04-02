module Arkham.Location.Cards.GlacialGrotto (glacialGrotto) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GlacialGrotto = GlacialGrotto LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glacialGrotto :: LocationCard GlacialGrotto
glacialGrotto = location GlacialGrotto Cards.glacialGrotto 3 (Static 0)

instance HasAbilities GlacialGrotto where
  getAbilities (GlacialGrotto attrs) =
    extendRevealed attrs []

instance RunMessage GlacialGrotto where
  runMessage msg (GlacialGrotto attrs) = runQueueT $ case msg of
    _ -> GlacialGrotto <$> liftRunMessage msg attrs
