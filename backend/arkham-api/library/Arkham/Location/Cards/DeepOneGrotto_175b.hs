module Arkham.Location.Cards.DeepOneGrotto_175b (
  deepOneGrotto_175b,
  DeepOneGrotto_175b (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype DeepOneGrotto_175b = DeepOneGrotto_175b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneGrotto_175b :: LocationCard DeepOneGrotto_175b
deepOneGrotto_175b = locationWith DeepOneGrotto_175b Cards.deepOneGrotto_175b 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities DeepOneGrotto_175b where
  getAbilities (DeepOneGrotto_175b attrs) =
    extendRevealed attrs []

instance RunMessage DeepOneGrotto_175b where
  runMessage msg (DeepOneGrotto_175b attrs) = runQueueT $ case msg of
    _ -> DeepOneGrotto_175b <$> liftRunMessage msg attrs
