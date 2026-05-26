module Arkham.Location.Cards.Barn (barn) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Barn = Barn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barn :: LocationCard Barn
barn = symbolLabel $ locationWith Barn Cards.barn 0 (Static 0) connectsToAdjacent

instance HasAbilities Barn where
  getAbilities (Barn a) =
    extendRevealed a []

instance RunMessage Barn where
  runMessage msg (Barn attrs) = runQueueT $ case msg of
    _ -> Barn <$> liftRunMessage msg attrs
