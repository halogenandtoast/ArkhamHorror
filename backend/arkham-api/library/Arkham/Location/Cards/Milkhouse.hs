module Arkham.Location.Cards.Milkhouse (milkhouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Milkhouse = Milkhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

milkhouse :: LocationCard Milkhouse
milkhouse = symbolLabel $ locationWith Milkhouse Cards.milkhouse 0 (Static 0) connectsToAdjacent

instance HasAbilities Milkhouse where
  getAbilities (Milkhouse a) =
    extendRevealed a []

instance RunMessage Milkhouse where
  runMessage msg (Milkhouse attrs) = runQueueT $ case msg of
    _ -> Milkhouse <$> liftRunMessage msg attrs
