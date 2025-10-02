module Arkham.Location.Cards.BigBen (bigBen) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BigBen = BigBen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bigBen :: LocationCard BigBen
bigBen = symbolLabel $ location BigBen Cards.bigBen 4 (PerPlayer 1)

instance HasAbilities BigBen where
  getAbilities (BigBen attrs) =
    extendRevealed attrs []

instance RunMessage BigBen where
  runMessage msg (BigBen attrs) = runQueueT $ case msg of
    _ -> BigBen <$> liftRunMessage msg attrs
