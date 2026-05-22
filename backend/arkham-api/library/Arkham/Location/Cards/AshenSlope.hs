module Arkham.Location.Cards.AshenSlope (ashenSlope) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AshenSlope = AshenSlope LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashenSlope :: LocationCard AshenSlope
ashenSlope = symbolLabel $ locationWith AshenSlope Cards.ashenSlope 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AshenSlope where
  getAbilities (AshenSlope a) =
    extendRevealed a []

instance RunMessage AshenSlope where
  runMessage msg (AshenSlope attrs) = runQueueT $ case msg of
    _ -> AshenSlope <$> liftRunMessage msg attrs
