module Arkham.Location.Cards.Tanneries (tanneries) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Tanneries = Tanneries LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tanneries :: LocationCard Tanneries
tanneries = symbolLabel $ location Tanneries Cards.tanneries 2 (PerPlayer 1)

instance HasAbilities Tanneries where
  getAbilities (Tanneries attrs) =
    extendRevealed attrs []

instance RunMessage Tanneries where
  runMessage msg (Tanneries attrs) = runQueueT $ case msg of
    _ -> Tanneries <$> liftRunMessage msg attrs
