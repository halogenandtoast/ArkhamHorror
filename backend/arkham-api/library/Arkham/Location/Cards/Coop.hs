module Arkham.Location.Cards.Coop (coop) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Coop = Coop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coop :: LocationCard Coop
coop = symbolLabel $ locationWith Coop Cards.coop 0 (Static 0) connectsToAdjacent

instance HasAbilities Coop where
  getAbilities (Coop a) =
    extendRevealed a []

instance RunMessage Coop where
  runMessage msg (Coop attrs) = runQueueT $ case msg of
    _ -> Coop <$> liftRunMessage msg attrs
